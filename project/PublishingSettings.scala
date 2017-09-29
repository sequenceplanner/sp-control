import sbt._
import sbt.{Developer, ScmInfo, url}
import sbt.Keys._
import SPSettings._

object PublishingSettings {
  lazy val description = "Sequence Planner (SP) is a micro service architecture for modeling and analyzing automation systems"
  lazy val organizationName = "sequenceplanner"
  lazy val comString = "com"
  lazy val githubString = "github"

  lazy val orgNameFull = comString +"."+ githubString +"."+ organizationName
  lazy val groupIdSonatype = comString +"."+ githubString +"."+ organizationName

  lazy val mitLicense = Seq("MIT License" -> url("https://opensource.org/licenses/MIT"))

  def githubSP(extra: String = "") = url("https://github.com/sequenceplanner/"+extra)
  def githubscm(extra: String = "")= "scm:git@github.com:sequenceplanner/" + extra

  val nexus = "https://oss.sonatype.org/"
  lazy val pubTo = Def.setting(
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  )






}

