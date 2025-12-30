ThisBuild / scalaVersion := "3.7.4"

lazy val jvm= project
  .in(file("scala"))
  .settings(
    Compile / unmanagedSourceDirectories := Seq(
      baseDirectory.value / ".." / "src",
      baseDirectory.value
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      "tools.aqua" % "z3-turnkey" % "4.14.1"
    )
  )
