import java.nio.file.{Files, Paths}

import SPSettings._

lazy val projectName = "sp-control"
lazy val projectVersion = "0.9.10"

lazy val domain = Def.setting(PublishingSettings.orgNameFull %%% "sp-domain" % "0.9.10")
lazy val comm = Def.setting(PublishingSettings.orgNameFull %%% "sp-comm" % "0.9.10")
lazy val gui =  Def.setting(PublishingSettings.orgNameFull %%% "sp-gui" % "0.9.10")
lazy val core = Def.setting(PublishingSettings.orgNameFull %%% "sp-core" % "0.9.6-SNAPSHOT")

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

lazy val serviceSettings = Seq(
  fork := true,
  javaOptions += s"-Dconfig.file=${root.base.getCanonicalPath}/cluster.conf",
  connectInput in run := true
)

lazy val root: Project = project.in(file("."))
  .aggregate(spcontrol_api_js, spcontrol_api_jvm, spcontrol_backend, spcontrol_frontend)
  .settings(defaultBuildSettings)
  .settings(buildSettings)
  .settings(
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file("")
    )


lazy val spcontrol_api = crossProject.crossType(CrossType.Pure).in(file("api"))
  .settings(
    defaultBuildSettings,
    buildSettings
  )

lazy val spcontrol_api_jvm = spcontrol_api.jvm
lazy val spcontrol_api_js = spcontrol_api.js

lazy val spcontrol_backend = project.in(file("backend"))
  .settings(
    libraryDependencies ++= commDependencies.value,
    libraryDependencies ++= Seq(comm.value, core.value, gui.value),
    defaultBuildSettings,
    buildSettings,
    serviceSettings,
    mainClass in (Compile, run) := Some("sp.Launch")
  )
  .dependsOn(spcontrol_api_jvm)


val SPGUILocation = "../sp-gui"
val UseLocalSPGUI = true

lazy val spcontrol_frontend = {
  if (UseLocalSPGUI && Files.exists(Paths.get(SPGUILocation))) {
    project.in(file("frontend"))
      .settings(
        libraryDependencies ++= Seq(comm.value),
        libraryDependencies ++= guiDependencies.value,
        defaultBuildSettings,
        buildSettings,
        jsSettings
      )
      .dependsOn(
        spcontrol_api_js,
        ProjectRef(file(SPGUILocation), "spguiJS") // This is where local version of spgui is set as dependency
      )
      .enablePlugins(ScalaJSPlugin)
  } else {
    project.in(file("frontend"))
      .settings(
        libraryDependencies ++= Seq(comm.value, gui.value),
        libraryDependencies ++= guiDependencies.value,
        defaultBuildSettings,
        buildSettings,
        jsSettings
      )
      .dependsOn(spcontrol_api_js)
      .enablePlugins(ScalaJSPlugin)
  }
}
