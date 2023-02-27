inThisBuild(
  List(
    name           := "quotidian",
    normalizedName := "quotidian",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.2.2",
    homepage       := Some(url("https://github.com/kitlangton/quotidian")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    )
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "quotidian",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"     % "2.0.9" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.0.9" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation"
    )
  )
