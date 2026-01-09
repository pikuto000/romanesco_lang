package romanesco
import scala.util.boundary, boundary.break

object Macro {
  private val macroRegistry = scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[(List[Node], List[Node])]]()
  private val tagCache = scala.collection.mutable.Map[String, HygenicTag]()
  
  def reset(): Unit = synchronized {
    macroRegistry.clear()
    tagCache.clear()
  }

  def register(name: String, pattern: List[Node], body: List[Node]): Boolean = synchronized {
    val entries = macroRegistry.getOrElseUpdate(name, scala.collection.mutable.ListBuffer())
    if (entries.exists(_._1 == pattern)) {
      false
    } else {
      (pattern, body) +=: entries
      true
    }
  }

  def expand(nodes: Array[Any]): Array[Any] = {
    logger.debug(s"Metamorphosis START (${nodes.length} nodes)")
    var current = nodes.toList
    var changed = true
    var pass = 0
    while (changed && pass < 10) {
      pass += 1
      val (next, passChanged) = expandList(current)
      current = next
      changed = passChanged
    }
    logger.debug(s"Metamorphosis FINISHED in $pass passes")
    current.toArray
  }

  private def expandList(nodes: List[Any]): (List[Any], Boolean) = {
    var anyChanged = false
    
    def process(list: List[Any]): List[Any] = {
      list match {
        case Nil => Nil
        case (node: Node) :: rest => {
          val (newChildren, childrenChanged) = expandList(node.children)
          if (childrenChanged) { anyChanged = true }
          val internal = node.copy(children = newChildren.collect { case n: Node => n })
          
          tryMatchAny(internal, rest) match {
            case Some((replacedNodes, remaining, matchedChanged)) => {
              if (matchedChanged) { anyChanged = true }
              replacedNodes ++ process(remaining)
            }
            case None => {
              internal :: process(rest)
            }
          }
        }
        case other :: rest => {
          other :: process(rest)
        }
      }
    }
    
    val result = process(nodes)
    (result, anyChanged)
  }

  private def tryMatchAny(firstNode: Node, rest: List[Any]): Option[(List[Any], List[Any], Boolean)] = synchronized {
    val optName = firstNode.kind match {
      case "MacroCall" => firstNode.getStr("name")
      case "Variable" => Some(firstNode.tag.name.split(":").lastOption.getOrElse(""))
      case _ => None
    }

    optName.flatMap { name =>
      macroRegistry.get(name).flatMap { definitions =>
        boundary {
          for ((pattern, body) <- definitions) {
            val (targets, tail) = if (firstNode.kind == "MacroCall") {
              (firstNode.children, rest)
            } else {
              collectNodes(rest, pattern.length)
            }

            if (targets.length == pattern.length) {
              val env = scala.collection.mutable.Map[String, Node]()
              if (matchAll(pattern, targets, env)) {
                val substitutedBody = body.map(b => substitute(b, env.toMap, freshLocals = true))
                break(Some((substitutedBody, tail, true)))
              }
            }
          }
          None
        }
      }
    }
  }

  private def matchAll(patterns: List[Node], targets: List[Node], env: scala.collection.mutable.Map[String, Node]): Boolean = {
    if (patterns.length != targets.length) {
      return false
    }
    patterns.zip(targets).forall { case (p, t) => matchNode(p, t, env) }
  }

  private def matchNode(p: Node, t: Node, env: scala.collection.mutable.Map[String, Node]): Boolean = {
    p.kind match {
      case "Variable" => {
        val varName = p.tag.name.split(":").lastOption.getOrElse("")
        env(varName) = t
        true
      }
      case _ if p.kind == t.kind => {
        matchAll(p.children, t.children, env)
      }
      case _ => false
    }
  }

  private def collectNodes(list: List[Any], count: Int): (List[Node], List[Any]) = {
    if (count <= 0) {
      (Nil, list)
    } else {
      list match {
        case (n: Node) :: rest => {
          val (collected, tail) = collectNodes(rest, count - 1)
          (n :: collected, tail)
        }
        case other :: rest => {
          collectNodes(rest, count)
        }
        case Nil => (Nil, Nil)
      }
    }
  }

  private def substitute(template: Node, env: Map[String, Node], freshLocals: Boolean): Node = {
    val localRefreshMap = scala.collection.mutable.Map[Int, HygenicTag]()
    def process(n: Node): Node = {
      n.kind match {
        case "Variable" => {
          val rawName = n.tag.name.split(":").lastOption.getOrElse("")
          if (env.contains(rawName)) {
            env(rawName)
          } else if (rawName.startsWith("@")) {
            val capturedName = rawName.substring(1)
            n.copy(tag = Hygenicmarker.bless(capturedName, None, false))
          } else if (freshLocals) {
            val newTag = localRefreshMap.getOrElseUpdate(n.tag.hash, Hygenicmarker.freshen(n.tag))
            n.copy(tag = newTag)
          } else {
            n
          }
        }
        case _ => {
          n.copy(children = n.children.map(process))
        }
      }
    }
    process(template)
  }

  object ConstantFolding {
    private val evaluator = new Evaluator()
    
    def apply(nodes: Array[Any]): Array[Any] = {
      nodes.map {
        case n: Node => fold(n)
        case o => o
      }
    }
    
    private def fold(n: Node): Node = {
      val foldedChildren = n.children.map(fold)
      val foldedNode = n.copy(children = foldedChildren)
      foldedNode.kind match {
        case "BinaryOp" if foldedChildren.length == 2 => {
          val left = foldedChildren(0)
          val right = foldedChildren(1)
          if (left.kind == "DecimalLiteral" && right.kind == "DecimalLiteral") {
            try {
              evaluator.eval(foldedNode) match {
                case Right(d: BigDecimal) => Node("DecimalLiteral", n.tag, Map("value" -> AttributeValue.Num(d)))
                case _ => foldedNode
              }
            } catch {
              case _: Exception => foldedNode
            }
          } else {
            foldedNode
          }
        }
        case _ => foldedNode
      }
    }
  }
}
