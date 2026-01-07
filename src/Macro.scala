package romanesco

import scala.util.boundary

object Macro {
  
  // マクロ定義: 名前 -> リスト[(パターン(Node), ボディ(Node))]
  // 引数名のリストではなく、パターンそのものを保持するように変更
  private val macroRegistry = scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[(List[Node], Node)]]()
  private val tagCache = scala.collection.mutable.Map[String, HygenicTag]()

  def register(name: String, patternArgs: List[Node], body: Node): Boolean = {
    logger.log(s"[macro.reg] Registering pattern for '$name' with ${patternArgs.length} args")
    val entries = macroRegistry.getOrElseUpdate(name, scala.collection.mutable.ListBuffer())
    // 同一パターンの重複登録を防止
    if (entries.exists(_._1 == patternArgs)) {
      logger.log(s"[macro.reg]   Pattern already exists for '$name'.")
      false
    } else {
      // 複雑なパターンを先に試すため、先頭に追加（Overloading）
      (patternArgs, body) +=: entries
      true
    }
  }

  def expand(nodes: Array[Any]): Array[Any] = {
    logger.log(s"[macro.expand] Metamorphosis starting on ${nodes.length} nodes")
    var current = nodes.toList
    var changed = true
    var pass = 0
    while (changed && pass < 10) {
      pass += 1
      val (next, passChanged) = expandStep(current)
      current = next
      changed = passChanged
    }
    current.toArray
  }

  private def expandStep(nodes: List[Any]): (List[Any], Boolean) = {
    var anyChanged = false
    def processList(list: List[Any]): List[Any] = {
      list match {
        case Nil => Nil
        case (node: Node) :: rest =>
          val (newChildren, childrenChanged) = expandStep(node.children)
          if (childrenChanged) anyChanged = true
          val nodeWithExpandedChildren = node.copy(children = newChildren.collect { case n: Node => n })
          val (finalNode, remaining, nodeChanged) = tryExpand(nodeWithExpandedChildren, rest)
          if (nodeChanged) anyChanged = true
          finalNode :: processList(remaining)
        case other :: rest => other :: processList(rest)
      }
    }
    (processList(nodes), anyChanged)
  }

  private def tryExpand(current: Node, rest: List[Any]): (Node, List[Any], Boolean) = {
    // マクロ名（MacroCallまたはVariable）を取得
    val optName = current.kind match {
      case "MacroCall" => Some(current.attributes("name").asInstanceOf[String])
      case "Variable"  => Some(current.tag.name.split(":").lastOption.getOrElse(""))
      case _ => None
    }

    optName match {
      case Some(name) =>
        macroRegistry.get(name) match {
          case Some(definitions) =>
            // 登録されているパターンを順に試行
            for ((patternArgs, body) <- definitions) {
              val (runtimeArgs, tail) = if (current.kind == "MacroCall") {
                (current.children, rest)
              } else {
                collectNodes(rest, patternArgs.length)
              }

              if (runtimeArgs.length == patternArgs.length) {
                // パターンマッチング開始
                val env = scala.collection.mutable.Map[String, Node]()
                if (matchAll(patternArgs, runtimeArgs, env)) {
                  logger.log(s"[macro.match] SUCCESS: '$name' matched pattern. Transforming...")
                  // マッチした環境でボディを置換（@による非衛生制御を含む）
                  val transformed = substitute(body, env.toMap, freshLocals = true)
                  boundary (transformed, tail, true)
                }
              }
            }
            (current, rest, false)
          case None => (current, rest, false)
        }
      case None => (current, rest, false)
    }
  }

  // 構造的なパターンマッチング
  private def matchAll(patterns: List[Node], targets: List[Node], env: scala.collection.mutable.Map[String, Node]): Boolean = {
    if (patterns.length != targets.length) return false
    patterns.zip(targets).forall { case (p, t) => matchNode(p, t, env) }
  }

  private def matchNode(p: Node, t: Node, env: scala.collection.mutable.Map[String, Node]): Boolean = {
    p.kind match {
      case "Variable" =>
        // パターン側の変数は「ワイルドカード」として機能し、ターゲットをキャプチャする
        val varName = p.tag.name.split(":").lastOption.getOrElse("")
        env(varName) = t
        true
      case _ if p.kind == t.kind =>
        // kindが一致する場合、子供も再帰的にマッチング
        matchAll(p.children, t.children, env)
      case _ => false
    }
  }

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

  // 置換ロジック: Hygieneの明示的制御
  private def substitute(template: Node, env: Map[String, Node], freshLocals: Boolean): Node = {
    val localRefreshMap = scala.collection.mutable.Map[Int, HygenicTag]()
    def process(n: Node): Node = {
      n.kind match {
        case "Variable" =>
          val rawName = n.tag.name.split(":").lastOption.getOrElse("")
          
          // 1. 引数置換
          if (env.contains(rawName)) {
            env(rawName)
          } 
          // 2. 非衛生的なキャプチャ（@prefix）
          else if (rawName.startsWith("@")) {
            // freshenせず、名前から '@' を除いた状態で元のタグを維持
            // これにより親スコープの変数と意図的に衝突させる
            val capturedName = rawName.substring(1)
            n.copy(tag = Hygenicmarker.bless(capturedName, None, false))
          }
          // 3. 衛生的なローカル変数（デフォルト）
          else if (freshLocals) {
            val newTag = localRefreshMap.getOrElseUpdate(n.tag.hash, Hygenicmarker.freshen(n.tag))
            n.copy(tag = newTag)
          } else n
        case _ =>
          n.copy(children = n.children.map(process))
      }
    }
    process(template)
  }

  object ConstantFolding {
    private val evaluator = new Evaluator()
    def apply(nodes: Array[Any]): Array[Any] = nodes.map { case n: Node => fold(n) case o => o }
    private def fold(n: Node): Node = {
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