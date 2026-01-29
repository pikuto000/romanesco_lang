val root = project
  .in(file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    scalaVersion := "3.8.1", 
    sbtVersion := "1.12.1", 
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala",
    Compile / unmanagedSources / excludeFilter := HiddenFileFilter || ((f: File) => f.getAbsolutePath.contains(".scala-build")),
    libraryDependencies ++=Seq( 
      "tools.aqua" % "z3-turnkey" % "4.14.1",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0"
    )
  )