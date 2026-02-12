import scala.scalanative.build._
lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.8.1",
    sbtVersion := "1.12.2",
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala",
    libraryDependencies ++= Seq()
  )
