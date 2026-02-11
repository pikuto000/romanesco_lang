package Utils

object OutputSettings {
  def setToUTF8: Unit = {
    System.setOut(new java.io.PrintStream(System.out, true, "UTF-8"))
    System.setErr(new java.io.PrintStream(System.err, true, "UTF-8"))
  }
}
