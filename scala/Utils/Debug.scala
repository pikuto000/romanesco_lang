package romanesco.Utils
//this object is debugging utilities.
object Debug {
  object logger {
    private var enable = false
    private var maxDepth = Int.MaxValue  // 追加: ログを出力する最大深さ
    private var currentDepth = 0         // 追加: 現在の深さ
    
    def switch(b: Boolean) = {
      enable = b
      if (enable) {
        println("[DEBUG] logger enabled")
      } else {
        println("[DEBUG] logger disabled")
      }
    }
    
    // 追加: 深さ制限を設定
    def setMaxDepth(depth: Int) = {
      maxDepth = depth
      println(s"[DEBUG] Max log depth set to $depth")
    }
    
    // 追加: 深さを増やす
    def increaseDepth() = {
      currentDepth += 1
    }
    
    // 追加: 深さを減らす
    def decreaseDepth() = {
      if (currentDepth > 0) currentDepth -= 1
    }
    
    // 追加: 深さをリセット
    def resetDepth() = {
      currentDepth = 0
    }
    
    def log(s: => String) = {
      if (enable && currentDepth <= maxDepth) {
        val indent = "  " * currentDepth
        println(s"$indent[DEBUG] $s")
      }
    }
  }
}
