import scala.scalanative.build._
lazy val root = (project in file("."))
  // .enablePlugins(ScalaNativePlugin)
  .settings(
    scalaVersion := "3.8.1",
    sbtVersion := "1.12.2",
    name := "romanesco",
    Compile / scalaSource := baseDirectory.value / "scala",
    libraryDependencies ++= Seq()
    /*nativeConfig ~= { c=>
      c.withLTO(LTO.none)
       .withMode(Mode.debug)
       .withGC(GC.commix)
       .withMultithreading(true)
    }*/
  )
