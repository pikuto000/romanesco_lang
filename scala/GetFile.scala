package romanesco
object GetFile {
  //Paths => File Contents
  def importfile(args: Array[String]): (Array[String], Array[String])={
    //オプション引数を検知
    val options = args.filter(arg => arg.startsWith("-"))
    val files = args.filter(arg => !arg.startsWith("-"))
    //コマンド引数をフルパスに変換
    val fullpath:Array[String]=files.map(
      arg => {
        val file = new java.io.File(arg)
        if (file.isAbsolute) file.getAbsolutePath
        else new java.io.File(currentPath + "/" + file).getAbsolutePath
      }
    )
    val FileContents = fullpath.map(
      arg => {
        val file = new java.io.File(arg)
        val fileString:String = new String(java.nio.file.Files.readAllBytes(file.toPath()))
        fileString
      }
    )
    (FileContents, options)
  }
  def currentPath = System.getProperty("user.dir")
}