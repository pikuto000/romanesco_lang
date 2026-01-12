ThisBuild / scalaVersion := "3.7.4"
ThisBuild / sbtVersion := "1.12.0"

lazy val jvm= project
  .in(file("scala"))
  .settings(
    Compile / unmanagedSourceDirectories := Seq(
      baseDirectory.value
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      "tools.aqua" % "z3-turnkey" % "4.14.1",
      "org.typelevel" %% "cats-core" % "2.9.0"
    )
  )
