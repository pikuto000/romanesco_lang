import scala.scalanative.build._
lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.8.2",
    sbtVersion := "1.12.4",
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala"
  )
