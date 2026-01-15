val spinalVersion = "1.13.0"
//
//lazy val root = (project in file("./scala")).
lazy val root = Project("root", file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.12.18",
      version      := "2.0.0"
    )),
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.12" % spinalVersion,
      "com.github.spinalhdl" % "spinalhdl-lib_2.12" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.12" % spinalVersion),
      "org.scalatest" %% "scalatest" % "3.2.17",
      "org.yaml" % "snakeyaml" % "1.8"
    ),
    name := "JaySoC",
    Compile / scalaSource := baseDirectory.value / "scala",
    javaHome := sys.env.get("JAVA_HOME") map file
    ).dependsOn(vexriscv)

lazy val vexriscv  = RootProject(file("../../VexRiscv"))

fork := true
