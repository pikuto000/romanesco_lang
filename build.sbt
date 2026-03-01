lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.8.2",
    sbtVersion := "1.12.3",
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala",
    // protoフォルダーを除外
    excludeFilter := "proto"
  )

lazy val proto = (project in file("proto"))
  .settings(
    scalaVersion := "3.8.2",
    sbtVersion := "1.12.3",
    Compile / scalaSource := baseDirectory.value / "Scala",
    name := "Proto_romanesco"
  )
