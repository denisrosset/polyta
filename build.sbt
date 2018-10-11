import ReleaseTransformations._

val alascVersion = "0.16.0.0"
val attributesVersion = "0.30"
val catsVersion = "1.1.0"
val catsEffectVersion = "1.0.0"
val disciplineVersion = "0.8"
val fastParseVersion = "1.0.0"
val proxVersion = "0.2.1"
val scalaCheckVersion = "1.13.5"
val scalaTestVersion = "3.0.5"
val scalinVersion = "0.16.0.0"
val shapelessVersion = "2.3.3"
val spireVersion = "0.16.0"

name := "polyta"

organization := "com.faacets"

scalaVersion := "2.12.7"

scmInfo := Some(ScmInfo(url("https://github.com/denisrosset/polyta"), "scm:git:git@github.com:denisrosset/polyta.git"))

homepage := Some(url("http://github.com/denisrosset/polyta"))

licenses += ("GPL-3.0", url("https://opensource.org/licenses/GPL-3.0"))

publishArtifact in Test := false

bintrayRepository := "maven"

resolvers ++= Seq(
  "bintray/non" at "http://dl.bintray.com/non/maven",
  "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.typelevel" %% "spire" % spireVersion,
  "net.alasc" %% "attributes" % attributesVersion,
  "net.alasc" %% "alasc-core" % alascVersion,
  "net.alasc" %% "scalin-core" % scalinVersion,
  "com.lihaoyi" %% "fastparse" % fastParseVersion,
  "io.github.vigoo" %% "prox" % proxVersion
)

scalacOptions ++= commonScalacOptions.diff(Seq(
  "-Xfatal-warnings",
  "-language:existentials",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
))

/*initialCommands in console := """
import com.faacets._; import qalg._; import polyta._
import spire.math.Rational; import qalg.math._
import Matrix.packs._
import qalg.syntax.all._
import qalg.syntax.algos.all._
import qalg.syntax.indup.all._
import net.alasc.math.{Grp, Perm}
import net.alasc.syntax.all._
"""
 */

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xfuture"
)
