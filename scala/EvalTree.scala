package Undeterminable

@main def testDrawTree(): Unit = 
  println("Evaluating drawTree method...")

  // Case 1: Simple Leaf
  val leafNode = tree.leaf("Leaf")
  println("\n--- Case 1: Simple Leaf ---")
  println(leafNode.drawTree)

  // Case 2: Tree with branches
  val branch1 = tree.leaf("B1")
  val branch2 = tree.leaf("B2")
  val root = tree.fork(List(branch1, branch2))
  println("\n--- Case 2: Tree with branches ---")
  println(root.drawTree)

  // Case 3: Nested tree
  val child = tree.single("Child", tree.leaf("GrandChild"))
  val parent = tree.single("Parent", child)
  println("\n--- Case 3: Nested tree ---")
  println(parent.drawTree)

  // Case 4: DAG (Diamond shape) - Testing shared node detection
  println("\n--- Case 4: DAG (Diamond) ---")
  val d = tree.leaf("D")
  val b = tree.single("B", d)
  val c = tree.single("C", d)
  val rootDAG = tree.fork(List(b, c))
  
  println(rootDAG.drawTree)

  // Case 5: Shared sub-branch (Multiple references)
  println("\n--- Case 5: Shared Sub-branch ---")
  val shared = tree.single("Shared", tree.leaf("End"))
  val r1 = tree.single("R1", shared)
  val r2 = tree.single("R2", shared)
  val rootShared = tree.fork(List(r1, r2))
  
  println(rootShared.drawTree)


