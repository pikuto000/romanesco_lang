package romanesco

object Macro {
  
  // 登録されたマクロ定義: 名前 -> (引数名リスト, ボディ)
  private val macroRegistry = scala.collection.mutable.Map[String, (List[String], Node)]()

  def register(name: String, args: List[String], body: Node): Unit = {
    macroRegistry(name) = (args, body)
    logger.log(s"[macro] Registered macro: $name with args $args")
  }

  // AST配列を走査し、マクロを結合・展開する
  def expand(nodes: Array[Any]): Array[Any] = {
    val results = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < nodes.length) {
      nodes(i) match {
        case node: Node if node.kind == "Variable" =>
          val varName = node.tag.name.split(":").lastOption.getOrElse("")
          macroRegistry.get(varName) match {
            case Some((args, body)) =>
              logger.log(s"[macro] Expanding macro: $varName with ${args.length} args")
              // 後続のノードから引数を取得
              // 非常に重要: Y = square 5 の場合、Unification(Y, square) と 5 に分かれている可能性がある
              // または、Y = square, 5 と並んでいる可能性がある
              val argValues = scala.collection.mutable.ArrayBuffer[Node]()
              var j = i + 1
              while (argValues.length < args.length && j < nodes.length) {
                nodes(j) match {
                  case n: Node => argValues += n
                  case _ => 
                }
                j += 1
              }

              if (argValues.length == args.length) {
                logger.log(s"[macro]   Found args: ${argValues.map(_.kind)}")
                results += substitute(body, args.zip(argValues).toMap)
                i = j - 1 // 進めた分だけインデックスを更新
              } else {
                results += node
              }
            case None =>
              results += node
          }
        
        case node: Node if node.kind == "Unification" =>
          // Unificationの右辺がマクロ呼び出しになっているケースを救う
          // 例: Unification(Variable(Y), Variable(square))
          node.children(1) match {
            case rhs: Node if rhs.kind == "Variable" =>
              val varName = rhs.tag.name.split(":").lastOption.getOrElse("")
              macroRegistry.get(varName) match {
                case Some((args, body)) =>
                  // 右辺がマクロ。引数を探す。
                  val argValues = scala.collection.mutable.ArrayBuffer[Node]()
                  var j = i + 1
                  while (argValues.length < args.length && j < nodes.length) {
                    nodes(j) match { case n: Node => argValues += n case _ => }
                    j += 1
                  }
                  if (argValues.length == args.length) {
                    val expandedRhs = substitute(body, args.zip(argValues).toMap)
                    results += node.copy(children = List(node.children(0), expandedRhs))
                    i = j - 1
                  } else results += node
                case None => results += node
              }
            case _ => results += node.copy(children = node.children.map(expandNode))
          }

        case node: Node if node.kind == "MacroDef" =>
          results += node

        case other =>
          results += other
      }
      i += 1
    }
    results.toArray
  }

  private def substitute(template: Node, env: Map[String, Node]): Node = {
    template.kind match {
      case "Variable" =>
        val name = template.tag.name.split(":").lastOption.getOrElse("")
        env.get(name).getOrElse(template)
      case _ =>
        template.copy(children = template.children.map(c => substitute(c, env)))
    }
  }

  private def expandNode(node: Node): Node = {
    node.kind match {
      case "Variable" =>
        val varName = node.tag.name.split(":").lastOption.getOrElse("")
        macroRegistry.get(varName) match {
          case Some((Nil, body)) => body
          case _ => node
        }
      case "BinaryOp" | "Unification" =>
        node.copy(children = node.children.map(expandNode))
      case _ => node
    }
  }

  object ConstantFolding {
    private val evaluator = new Evaluator()
    def apply(nodes: Array[Any]): Array[Any] = {
      nodes.map {
        case node: Node => fold(node)
        case other => other
      }
    }
    private def fold(node: Node): Node = {
      val foldedChildren = node.children.map(fold)
      val foldedNode = node.copy(children = foldedChildren)
      foldedNode.kind match {
        case "BinaryOp" =>
          if (foldedChildren.length == 2) {
            val left = foldedChildren(0)
            val right = foldedChildren(1)
            if (left.kind == "DecimalLiteral" && right.kind == "DecimalLiteral") {
              try {
                val result = evaluator.eval(foldedNode)
                result match {
                  case d: BigDecimal =>
                    logger.log(s"[macro] Folded: $foldedNode -> $d")
                    Node("DecimalLiteral", node.tag, Map("value" -> d))
                  case _ => foldedNode
                }
              } catch { case _: Exception => foldedNode }
            } else foldedNode
          } else foldedNode
        case _ => foldedNode
      }
    }
  }
}
