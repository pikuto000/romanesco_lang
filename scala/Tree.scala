package Undeterminable

/**
 * 非決定的な計算を表現する木構造
 * レキサー・パーサーの複数の可能性を効率的に管理する
 */
enum tree[+A]:
  case Node(values: Vector[A], branches: List[tree[A]])
  case DeadEnd

  /**
   * 全ての可能なパスを遅延リストとして取得
   * メモリ効率的に全候補を列挙
   */
  def flattenPaths: List[List[A]] = this match
    case DeadEnd => 
      List.empty
    
    case Node(vals, branches) if branches.isEmpty =>
      List(List.from(vals))
    
    case Node(vals, branches) =>
      val prefix = List.from(vals)
      branches.flatMap(_.flattenPaths.map(prefix ++ _))

  /**
   * デバッグ用：木構造を視覚化
   * 循環参照を検出してループを防ぐ
   */
  def drawTree: String =
    val visited = scala.collection.mutable.Set.empty[Int]
    
    def buildChain(t: tree[A]): (String, List[tree[A]]) =
      var curr = t
      val chain = new StringBuilder()
      var branches: List[tree[A]] = List.empty
      var stop = false
      
      while !stop do
        val id = System.identityHashCode(curr)
        
        if visited.contains(id) then
          if chain.nonEmpty then chain.append(" -> ")
          chain.append(s"(ref: @${Integer.toHexString(id)})")
          stop = true
        else
          visited += id
          if chain.nonEmpty then chain.append(" -> ")
          
          curr match
            case DeadEnd =>
              chain.append("DeadEnd")
              stop = true
            
            case Node(vals, brs) =>
              chain.append(s"${vals.mkString(", ")} @${Integer.toHexString(id)}")
              brs match
                case Nil | List() => // List() added to handle List.empty explicitly
                  stop = true
                case h :: Nil => // Changed to h :: Nil for single element list
                  curr = h
                case _ =>
                  branches = brs
                  stop = true
      
      (chain.toString, branches)
    
    def loop(t: tree[A], prefix: String, isLast: Boolean, isRoot: Boolean): String =
      val (chainStr, branches) = buildChain(t)
      
      val (marker, childIndent) =
        if isRoot then ("", "")
        else if isLast then ("└── ", "    ")
        else ("├── ", "│   ")
      
      val header = s"$prefix$marker$chainStr"
      
      if branches.isEmpty then
        header + "\n"
      else
        val body = branches.zipWithIndex.map { (b, i) =>
          loop(b, prefix + childIndent, i == branches.length - 1, false)
        }.mkString
        header + "\n" + body
    
    loop(this, "", isLast = true, isRoot = true)

object tree:
  /** 単一の値を持つ葉ノード */
  def leaf[A](value: A): tree[A] =
    Node(Vector(value), List.empty)
  
  /** 空のノード（分岐点） */
  def fork[A](branches: List[tree[A]]): tree[A] =
    Node(Vector.empty, branches)
  
  /** 単一の分岐を持つノード */
  def single[A](value: A, next: tree[A]): tree[A] =
    Node(Vector(value), List(next))
