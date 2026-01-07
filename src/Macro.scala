package romanesco
import scala.util.boundary, boundary.break

object Macro {
  
  private val macroRegistry = scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[(List[Node], List[Node])]]()
  private val evaluator = new Evaluator()

  def register(name: String, pattern: List[Node], body: List[Node]): Boolean = synchronized {
    val entries = macroRegistry.getOrElseUpdate(name, scala.collection.mutable.ListBuffer())
    if (entries.exists(_._1 == pattern)) false
    else {
      (pattern, body) +=: entries
      true
    }
  }

  def expand(nodes: Array[Any]): Array[Any] = {
    var current = nodes.toList
    var changed = true
    var pass = 0
    while (changed && pass < 10) {
      pass += 1
      val (next, passChanged) = expandList(current)
      current = next
      changed = passChanged
    }
    current.toArray
  }

  private def expandList(nodes: List[Any]): (List[Any], Boolean) = {
    var anyChanged = false
    
    def process(list: List[Any]): List[Any] = {
      list match {
        case Nil => Nil
        case (node: Node) :: rest =>
          val (newChildren, childrenChanged) = expandList(node.children)
          if (childrenChanged) anyChanged = true
          val internal = tryFold(node.copy(children = newChildren.collect { case n: Node => n }))

          tryMatchMixfix(internal, rest) match {
            case Some((replacedNodes, remaining, matchedChanged)) =>
              if (matchedChanged) anyChanged = true
              replacedNodes ++ process(remaining)
            case None =>
              internal :: process(rest)
          }
        case other :: rest => 
          other :: process(rest)
      }
    }
    
    val result = process(nodes)
    (result, anyChanged)
  }

  private def tryMatchMixfix(firstNode: Node, rest: List[Any]): Option[(List[Any], List[Any], Boolean)] = synchronized {
    for ((name, definitions) <- macroRegistry) {
      for ((pattern, body) <- definitions) {
        val env = scala.collection.mutable.Map[String, Node]()
        val (targets, tail) = collectNodes(firstNode :: rest, pattern.length)
        
        if (targets.length == pattern.length && matchAll(pattern, targets, env)) {
          // 呼び出し元（firstNode）のタグのハッシュを種にする
          val seed = firstNode.tag.hash
          
          val substitutedBody = body.map(b => 
            tryFold(substitute(b, env.toMap, freshLocals = true, seed))
          )
          return Some((substitutedBody, tail, true))
        }
      }
    }
    None
  }

  private def matchAll(patterns: List[Node], targets: List[Node], env: scala.collection.mutable.Map[String, Node]): Boolean = {
    patterns.zip(targets).forall { case (p, t) => matchNode(p, t, env) }
  }

  private def matchNode(p: Node, t: Node, env: scala.collection.mutable.Map[String, Node]): Boolean = {
    p.kind match {
      case "Variable" =>
        val varName = p.tag.name.split(":").lastOption.getOrElse("")
        env(varName) = t
        true
      case "LiteralWord" if t.kind == "Variable" =>
        p.attributes("value") == t.tag.name.split(":").lastOption.getOrElse("")
      case _ if p.kind == t.kind =>
        if (p.kind == "LiteralWord") p.attributes("value") == t.attributes("value")
        else matchAll(p.children, t.children, env)
      case _ => false
    }
  }

  private def collectNodes(list: List[Any], count: Int): (List[Node], List[Any]) = {
    if (count <= 0) (Nil, list)
    else list match {
      case (n: Node) :: rest => 
        val (collected, tail) = collectNodes(rest, count - 1)
        (n :: collected, tail)
      case other :: rest => collectNodes(rest, count)
      case Nil => (Nil, Nil)
    }
  }

  private def substitute(template: Node, env: Map[String, Node], freshLocals: Boolean, seed: Int): Node = {
    val localRefreshMap = scala.collection.mutable.Map[Int, HygenicTag]()
    def process(n: Node): Node = {
      val res = n.kind match {
        case "Variable" =>
          val rawName = n.tag.name.split(":").lastOption.getOrElse("")
          if (env.contains(rawName)) env(rawName)
          else if (rawName.startsWith("@")) {
            val capturedName = rawName.substring(1)
            n.copy(tag = Hygenicmarker.bless(capturedName, None, false))
          } else if (freshLocals) {
            // seed を使って一意化
            val newTag = localRefreshMap.getOrElseUpdate(n.tag.hash, Hygenicmarker.freshen(n.tag, seed))
            n.copy(tag = newTag)
          } else n
        case _ =>
          n.copy(children = n.children.map(process))
      }
      tryFold(res)
    }
    process(template)
  }

  private def tryFold(n: Node): Node = {
    n.kind match {
      case "BinaryOp" if n.children.forall(_.kind == "DecimalLiteral") =>
        try {
          evaluator.eval(n) match {
            case d: BigDecimal => Node("DecimalLiteral", n.tag, Map("value" -> d))
            case _ => n
          }
        } catch { case _: Exception => n }
      case _ => n
    }
  }

  object ConstantFolding {
    def apply(nodes: Array[Any]): Array[Any] = nodes.map { case n: Node => tryFold(n) case o => o }
  }
}
