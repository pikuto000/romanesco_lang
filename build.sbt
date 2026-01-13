ThisBuild / scalaVersion := "3.7.4"
ThisBuild / sbtVersion := "1.12.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lexer",
    Compile / scalaSource := baseDirectory.value / "scala",
    run / mainClass := Some("Main")
  )
