package romanesco

object Macro {
  
  private val macroRegistry = scala.collection.mutable.Map[String, (List[String], Node)]()
  private val tagCache = scala.collection.mutable.Map[String, HygenicTag]()

  def getOrCreateTag(name: String, parser: Parser): HygenicTag = {
    logger.log(s"[macro.tag] getOrCreateTag: $name")
    tagCache.getOrElseUpdate(name, {
      logger.log(s"[macro.tag]   Cache miss. Blessing new tag for $name")
      Hygenicmarker.bless(name, Some(parser), true)
    })
  }

  def register(name: String, args: List[String], body: Node): Boolean = {
    logger.log(s"[macro.reg] register: $name with args $args")
    val newData = (args, body)
    if (macroRegistry.get(name).contains(newData)) {
      logger.log(s"[macro.reg]   Registration SKIPPED (identical data exists)")
      false 
    } else {
      logger.log(s"[macro.reg]   Registering NEW macro: $name")
      macroRegistry(name) = newData
      true 
    }
  }

  def expand(nodes: Array[Any]): Array[Any] = {
    logger.log(s"[macro.expand] Expanding ${nodes.length} root nodes...")
    val res = expandNodes(nodes.toList).toArray
    logger.log(s"[macro.expand] Final expanded count: ${res.length}")
    res
  }

  private def expandNodes(nodes: List[Any]): List[Any] = {
    nodes match {
      case Nil => 
        logger.log("[macro.expand] Nil nodes encountered")
        Nil
      case (node: Node) :: rest =>
        logger.log(s"[macro.node] Visiting node: kind=${node.kind}, children=${node.children.length}")
        
        // 1. まず現在のノード自体を再帰的に展開
        val internalExpanded = node.copy(children = expandNodes(node.children).collect { case n: Node => n })
        
        // 2. マクロ呼び出しとしての展開を試みる
        val (finalNode, remaining) = tryExpand(internalExpanded, rest)
        
        finalNode :: expandNodes(remaining)
      case other :: rest => 
        logger.log(s"[macro.node] Skipping non-node element: $other")
        other :: expandNodes(rest)
    }
  }

  private def tryExpand(current: Node, rest: List[Any]): (Node, List[Any]) = {
    current.kind match {
      case "Variable" =>
        val name = current.tag.name.split(":").lastOption.getOrElse("")
        logger.log(s"[macro.try] Checking variable '$name' for macro lookup")
        macroRegistry.get(name) match {
          case Some((argNames, body)) if argNames.nonEmpty =>
            logger.log(s"[macro.try]   Macro '$name' found in registry! Attempting to collect ${argNames.length} args...")
            val (args, tail) = collectNodes(rest, argNames.length)
            if (args.length == argNames.length) {
              logger.log(s"[macro.try]   Args collected: ${args.map(_.kind)}. Substituting...")
              (substitute(body, argNames.zip(args).toMap), tail)
            } else {
              logger.log(s"[macro.try]   Insufficient args in stream (found ${args.length}, need ${argNames.length}). Keeping as variable.")
              (current, rest)
            }
          case _ => 
            logger.log(s"[macro.try]   No matching macro for '$name'")
            (current, rest)
        }
      case "MacroCall" =>
        val name = current.attributes("name").asInstanceOf[String]
        logger.log(s"[macro.try] Found direct MacroCall: $name. Children: ${current.children.length}")
        macroRegistry.get(name) match {
          case Some((argNames, body)) =>
            logger.log(s"[macro.try]   Macro '$name' data found. Substituting children...")
            val result = substitute(body, argNames.zip(current.children).toMap)
            (result, rest)
          case None => 
            logger.log(s"[macro.try]   Warning: Macro $name NOT found in registry")
            (current, rest)
        }
      case "Unification" if current.children.length == 2 =>
        // 右辺がマクロ呼び出しになっているケース: Y = square 5
        val lhs = current.children(0)
        val rhs = current.children(1)
        if (rhs.kind == "Variable") {
          val name = rhs.tag.name.split(":").lastOption.getOrElse("")
          macroRegistry.get(name) match {
            case Some((argNames, body)) if argNames.nonEmpty =>
              logger.log(s"[macro.try] Found macro '$name' on right side of Unification. Looking for args in rest...")
              val (args, tail) = collectNodes(rest, argNames.length)
              if (args.length == argNames.length) {
                logger.log(s"[macro.try]   Args collected from sibling stream. Expanding...")
                val expandedRhs = substitute(body, argNames.zip(args).toMap)
                (current.copy(children = List(lhs, expandedRhs)), tail)
              } else {
                (current, rest)
              }
            case _ => (current, rest)
          }
        } else (current, rest)
      case _ => 
        (current, rest)
    }
  }

  private def collectNodes(list: List[Any], count: Int): (List[Node], List[Any]) = {
    if (count <= 0) (Nil, list)
    else list match {
      case (n: Node) :: rest => 
        val (collected, tail) = collectNodes(rest, count - 1)
        (n :: collected, tail)
      case _ :: rest => 
        logger.log("[macro.coll] Skipping non-node during arg collection")
        collectNodes(rest, count) 
      case Nil => 
        (Nil, Nil)
    }
  }

  private def substitute(template: Node, env: Map[String, Node]): Node = {
    template.kind match {
      case "Variable" =>
        val name = template.tag.name.split(":").lastOption.getOrElse("")
        env.get(name) match {
          case Some(replacement) => 
            logger.log(s"[macro.subst]   Substituting '$name' -> kind=${replacement.kind}")
            replacement
          case None => 
            logger.log(s"[macro.subst]   Keeping original variable '$name'")
            template
        }
      case _ =>
        if (template.children.nonEmpty) {
          template.copy(children = template.children.map(c => substitute(c, env)))
        } else template
    }
  }

  object ConstantFolding {
    private val evaluator = new Evaluator()
    def apply(nodes: Array[Any]): Array[Any] = {
      logger.log(s"[macro.fold] ConstantFolding starting on ${nodes.length} nodes")
      val res = nodes.map {
        case node: Node => fold(node)
        case other => other
      }
      logger.log("[macro.fold] ConstantFolding finished")
      res
    }
    private def fold(node: Node): Node = {
      val foldedChildren = node.children.map(fold)
      val foldedNode = node.copy(children = foldedChildren)
      foldedNode.kind match {
        case "BinaryOp" if foldedChildren.length == 2 =>
          val left = foldedChildren(0); val right = foldedChildren(1)
          if (left.kind == "DecimalLiteral" && right.kind == "DecimalLiteral") {
            try {
              evaluator.eval(foldedNode) match {
                case d: BigDecimal => 
                  logger.log(s"[macro.fold] FOLDED node at kind=${node.kind} to literal $d")
                  Node("DecimalLiteral", node.tag, Map("value" -> d))
                case _ => foldedNode
              }
            } catch { case _: Exception => foldedNode }
          } else foldedNode
        case _ => foldedNode
      }
    }
  }
}
