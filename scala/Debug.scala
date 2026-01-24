package romanesco
//this object is debugging utilities.
object Debug {
  object logger{
    private var enable=false
    def switch(b:Boolean)={
      enable=b
      if (enable){
        println("logger enabled")
      }else{
        println("logger disabled")
      }
    }
    def log(s: =>String)={
      if (enable){
        println(s)
      }
    }
  }
}
