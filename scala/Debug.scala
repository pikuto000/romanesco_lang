package romanesco
//this object is debugging utilities.
object Debug {
  object logger{
    private var enable=false
    def switch(b:Boolean)={
      enable=b
      if (enable){
        println("[DEBUG] logger enabled")
      }else{
        println("[DEBUG] logger disabled")
      }
    }
    def log(s: =>String)={
      if (enable){
        println(s"[DEBUG] $s")
      }
    }
  }
}
