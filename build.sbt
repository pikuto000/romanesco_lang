ThisBuild / scalaVersion := "3.7.4"

lazy val jvm= project
  .in(file("scala"))
  .settings(
    Compile / unmanagedSourceDirectories := Seq(
      baseDirectory.value / ".." / "src",
      baseDirectory.value
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  )