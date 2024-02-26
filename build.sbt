inThisBuild(
  List(
    name           := "quotidian",
    normalizedName := "quotidian",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.3.1",
    homepage       := Some(url("https://github.com/kitlangton/quotidian")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    )
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (crossProject(JSPlatform, JVMPlatform) in file("."))
  .settings(
    name := "root"
  )
  .aggregate(core, examples)

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("modules/core"))
  .settings(
    name := "quotidian",
    libraryDependencies ++= Seq(
      "dev.zio"     %% "zio-test"     % "2.1-RC1" % Test,
      "dev.zio"     %% "zio-test-sbt" % "2.1-RC1" % Test,
      "com.lihaoyi" %% "pprint"       % "0.8.1"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-Xcheck-macros"
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  )
  .enablePlugins(ScalaJSPlugin)

lazy val examples = (crossProject(JVMPlatform) in file("examples"))
  .settings(
    name := "quotidian-examples",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % "2.1-RC1",
      "dev.zio" %% "zio-test"     % "2.1-RC1" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1-RC1" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-Xcheck-macros"
    )
  )
  // .jsSettings(
  //   scalaJSUseMainModuleInitializer := true,
  //   scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  // )
  .dependsOn(core)
// .enablePlugins(ScalaJSPlugin)
