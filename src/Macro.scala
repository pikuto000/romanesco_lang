package romanesco

object Macro {
  
  private val macroRegistry = scala.collection.mutable.Map[String, (List[String], Node)]()
  private val tagCache = scala.collection.mutable.Map[String, HygenicTag]()

  def getOrCreateTag(name: String, parser: Parser): HygenicTag = {
    tagCache.getOrElseUpdate(name, Hygenicmarker.bless(name, Some(parser), true))
  }

  def register(name: String, args: List[String], body: Node): Boolean = {
    val newData = (args, body)
    if (macroRegistry.get(name).contains(newData)) false 
    else { macroRegistry(name) = newData; logger.log(s"[macro] Reg: $name"); true }
  }

  def expand(nodes: Array[Any]): Array[Any] = {
    logger.log(s"[macro] Expanding ${nodes.length} nodes")
    expandNodesStream(nodes.toList).toArray
  }

  // 最適化: テールリカージョンを意識したストリーム処理
  private def expandNodesStream(nodes: List[Any]): List[Any] = {
    if (nodes.isEmpty) return Nil
    
    nodes match {
      case (node: Node) :: rest =>
        val internal = node.copy(children = expandNodesStream(node.children).collect { case n: Node => n })
        val (finalNode, remaining) = tryExpand(internal, rest)
        finalNode :: expandNodesStream(remaining)
      case other :: rest => 
        other :: expandNodesStream(rest)
      case Nil =>
        throw new Exception("cannot expand because of nil") 
    }
  }

  private def tryExpand(current: Node, rest: List[Any]): (Node, List[Any]) = {
    current.kind match {
      case "Variable" =>
        val name = current.tag.name.split(":").lastOption.getOrElse("")
        macroRegistry.get(name) match {
          case Some((argNames, body)) if argNames.nonEmpty =>
            val (args, tail) = collectNodes(rest, argNames.length)
            if (args.length == argNames.length) {
              (substitute(body, argNames.zip(args).toMap), tail)
            } else (current, rest)
          case _ => (current, rest)
        }
      case "MacroCall" =>
        val name = current.attributes("name").asInstanceOf[String]
        macroRegistry.get(name).map { case (argNames, body) =>
          (substitute(body, argNames.zip(current.children).toMap), rest)
        }.getOrElse((current, rest))
      case "Unification" if current.children.length == 2 =>
        val lhs = current.children(0); val rhs = current.children(1)
        if (rhs.kind == "Variable") {
          val name = rhs.tag.name.split(":").lastOption.getOrElse("")
          macroRegistry.get(name).collect { 
            case (argNames, body) if argNames.nonEmpty =>
              val (args, tail) = collectNodes(rest, argNames.length)
              if (args.length == argNames.length) {
                (current.copy(children = List(lhs, substitute(body, argNames.zip(args).toMap))), tail)
              } else null
          }.getOrElse((current, rest)) match { case null => (current, rest) case r => r }
        } else (current, rest)
      case _ => (current, rest)
    }
  }

  // 最適化: 再帰による引数収集（リストの先頭から count 分だけ Node を奪う）
  private def collectNodes(list: List[Any], count: Int): (List[Node], List[Any]) = {
    if (count <= 0) (Nil, list)
    else list match {
      case (n: Node) :: rest => 
        val (collected, tail) = collectNodes(rest, count - 1)
        (n :: collected, tail)
      case _ :: rest => collectNodes(rest, count) 
      case Nil => (Nil, Nil)
    }
  }

  private def substitute(template: Node, env: Map[String, Node]): Node = {
    template.kind match {
      case "Variable" =>
        val name = template.tag.name.split(":").lastOption.getOrElse("")
        env.getOrElse(name, template)
      case _ =>
        if (template.children.nonEmpty) template.copy(children = template.children.map(c => substitute(c, env)))
        else template
    }
  }

  object ConstantFolding {
    private val evaluator = new Evaluator()
    def apply(nodes: Array[Any]): Array[Any] = nodes.map { case n: Node => fold(n) case o => o }
    private def fold(n: Node): Node = {
      if (n.children.isEmpty) return n
      val foldedChildren = n.children.map(fold)
      val foldedNode = n.copy(children = foldedChildren)
      foldedNode.kind match {
        case "BinaryOp" if foldedChildren.length == 2 =>
          val left = foldedChildren(0); val right = foldedChildren(1)
          if (left.kind == "DecimalLiteral" && right.kind == "DecimalLiteral") {
            try {
              evaluator.eval(foldedNode) match {
                case d: BigDecimal => Node("DecimalLiteral", n.tag, Map("value" -> d))
                case _ => foldedNode
              }
            } catch { case _: Exception => foldedNode }
          } else foldedNode
        case _ => foldedNode
      }
    }
  }
}