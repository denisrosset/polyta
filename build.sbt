// inspired by Spire build.sbt file

val attributesVersion = "0.11.0.1"
val alascVersion = "0.11.0.3-SNAPSHOT"
val disciplineVersion = "0.4"
val fastParseVersion = "0.3.7"
val metalVersion = "0.11.0.5-SNAPSHOT"
val scalaCheckVersion = "1.12.4"
val scalaTestVersion = "3.0.0-M7"
val scalinVersion = "0.11.0.5-SNAPSHOT"
val shapelessVersion = "2.2.5"
val spireVersion = "0.11.0"

name := "Polyta"

organization := "com.faacets"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "bintray/non" at "http://dl.bintray.com/non/maven",
  "bintray/denisrosset/net.alasc" at "https://dl.bintray.com/denisrosset/net.alasc",
  "bintray/denisrosset/metal" at "https://dl.bintray.com/denisrosset/metal",
  "bintray/denisrosset/scalin" at "https://dl.bintray.com/denisrosset/scalin",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.spire-math" %% "spire" % spireVersion,
  "net.alasc" %% "attributes" % attributesVersion,
  "net.alasc" %% "alasc-core" % alascVersion,
  "net.alasc" %% "alasc-scalin" % alascVersion,
  "net.alasc" %% "scalin-core" % scalinVersion,
  "com.lihaoyi" %% "fastparse" % fastParseVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
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
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xfuture"
)
