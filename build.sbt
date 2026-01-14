ThisBuild / scalaVersion := "3.7.4"
ThisBuild / sbtVersion := "1.12.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala",
    Compile / unmanagedSources / excludeFilter := HiddenFileFilter || ((f: File) => f.getAbsolutePath.contains(".scala-build")),
    libraryDependencies += "tools.aqua" % "z3-turnkey" % "4.14.1"
  )
