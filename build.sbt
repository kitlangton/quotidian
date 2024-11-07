inThisBuild(
  List(
    name           := "quotidian",
    normalizedName := "quotidian",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.3.4",
    homepage       := Some(url("https://github.com/kitlangton/quotidian")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

////////////////////////
// sbt-github-actions //
////////////////////////
ThisBuild / githubWorkflowJavaVersions += JavaSpec.temurin("17")

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v")),
    RefPredicate.Equals(Ref.Branch("main"))
  )

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

/////////////////////////
// Project Definitions //
/////////////////////////

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name           := "root",
    publish / skip := true
  )
  .aggregate(
    core,
    examples
  )

lazy val core =
  (crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure) in file("modules/core"))
    .settings(
      name := "quotidian",
      libraryDependencies ++= Seq(
        "dev.zio"     %% "zio-test"     % "2.1.12" % Test,
        "dev.zio"     %% "zio-test-sbt" % "2.1.12" % Test,
        "com.lihaoyi" %% "pprint"       % "0.9.0"
      ),
      scalacOptions ++= Seq(
        "-deprecation",
        "-Xcheck-macros"
        // "-Wunused:all"
      )
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
    )

lazy val examples = (crossProject(JVMPlatform).crossType(CrossType.Pure) in file("examples"))
  .settings(
    name           := "quotidian-examples",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % "2.1.12",
      "dev.zio" %% "zio-test"     % "2.1.12" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.12" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-Xcheck-macros"
    )
  )
//  .jsSettings(
//    scalaJSUseMainModuleInitializer := true,
//    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
//  )
  .dependsOn(core)
//  .enablePlugins(ScalaJSPlugin)

addCommandAlias("prepare", "scalafmtAll;scalafixAll;githubWorkflowGenerate")
