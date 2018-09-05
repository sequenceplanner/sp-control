import SPSettings._

lazy val projectName = "sp-control"
lazy val projectVersion = "0.9.11"

lazy val coreServer = Def.setting(PublishingSettings.orgNameFull %%% "sp-core-server" % "testingLocal-SNAPSHOT")
lazy val coreClient = Def.setting(PublishingSettings.orgNameFull %%% "sp-core-client" % "testingLocal-SNAPSHOT")
lazy val domain = Def.setting(PublishingSettings.orgNameFull %%% "sp-domain" % "0.9.12")
lazy val comm = Def.setting(PublishingSettings.orgNameFull %%% "sp-comm" % "0.9.11")
lazy val gui =  Def.setting(PublishingSettings.orgNameFull %%% "sp-gui" % "0.9.11")

lazy val buildSettings = Seq(
  name         := projectName,
  description  := "Sequence Planner, with a focus on automation",
  version      := projectVersion,
  libraryDependencies ++= domainDependencies.value,
  libraryDependencies += domain.value,
  scmInfo := Some(ScmInfo(
    PublishingSettings.githubSP(projectName),
    PublishingSettings.githubscm(projectName)
    )
  )
)
/*
lazy val serviceSettings = Seq(
  fork := true,
  javaOptions += s"-Dconfig.file=${baseDirectory.in(root).value.getCanonicalPath}/cluster.conf",
  connectInput in run := true
)
*/

lazy val root: Project = project.in(file("."))
  .aggregate(sharedJs, sharedJvm, server, client)
  .settings(defaultBuildSettings)
  .settings(buildSettings)
  .settings(
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file("")
    )


lazy val shared = crossProject.crossType(CrossType.Pure).in(file("api"))
  .settings(
    defaultBuildSettings,
    buildSettings
  )

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js


lazy val server: Project = project.in(file("backend"))
  .settings(
    Keys.javaOptions += s"-Dconfig.file=${baseDirectory.value.getAbsoluteFile \ "backend" \ "src" \ "main" \ "resources" \ "application.conf"}",
    libraryDependencies ++= commDependencies.value,
    libraryDependencies ++= Seq(comm.value, coreServer.value),
    defaultBuildSettings,
    buildSettings
//    ,serviceSettings
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback, PlayLayoutPlugin)
  .dependsOn(sharedJvm)


lazy val client = project.in(file("frontend"))
  .settings(
    libraryDependencies ++= Seq(comm.value, coreClient.value),
    libraryDependencies ++= guiDependencies.value,
    defaultBuildSettings,
    buildSettings,
    jsSettings
  )
  .dependsOn(sharedJs)
  .enablePlugins(ScalaJSPlugin)
