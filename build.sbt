lazy val root = (project in file(".")).settings(
  name := "pso_linear_combinations",
  version := "0.2",
  scalaVersion := "2.13.0",
  mainClass in Compile := Some("pso.Main")
)

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0",
  "com.lihaoyi" %% "ujson" % "0.9.5"

)

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 13 =>
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
    case _ =>
      Seq()
  }
}
