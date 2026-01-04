package romanesco

object logger {
  //これはデバッグ用
  private var Enable=false
  def Switch(enable:Boolean):Unit={
    Enable=enable
    println(s"logger is $Enable")
  }
  def log(s:String):Unit={
    if (Enable){
      println(s"[logger] $s")
    }
  }
}
