import sbt.Def
import sbt.Keys._
import sbt.Project
import sbt._
import sbtbuildinfo.BuildInfoPlugin
import scoverage.ScoverageSbtPlugin
import com.reactific.sbt.ProjectPlugin
import com.reactific.sbt.ProjectPlugin.autoImport._

object CBORiSBuild extends Build with Dependencies {
  val classesIgnoredByScoverage : String = Seq[String](
    "<empty>", // Avoids warnings from scoverage
    "com.reactific.cboris.ComReactificCborisInfo"
  ).mkString(";")

  val base_name = "cboris"
  val buildSettings: Seq[Def.Setting[_]] = Seq(
    // credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    organization := "com.reactific",
    scalaVersion := Ver.scala,
    maxErrors := 50,
    ScoverageSbtPlugin.ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := classesIgnoredByScoverage,
    resolvers ++= all_resolvers,
    copyrightHolder := "Reactific Software LLC",
    copyrightYears := Seq(2015),
    developerUrl := url("http://reactific.com/")
  )

  lazy val proj = Project(base_name, file("."))
    .enablePlugins(ProjectPlugin, BuildInfoPlugin)
    .settings(buildSettings: _*)
    .settings(
      titleForDocs := "CBOR in Scala",
      codePackage := "com.reactific.cboris",
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 95,
      libraryDependencies ++= root_dependencies
    )

}
