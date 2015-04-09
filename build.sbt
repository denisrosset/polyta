name := "Polyta"

organization := "com.faacets"

version := "0.9.2-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.spire-math" %% "spire" % "0.9.2-SNAPSHOT",
  "net.alasc" %% "alasc" % "0.9.2-SNAPSHOT",
  "net.alasc" %% "alasc-qalg-binding" % "0.9.2-SNAPSHOT",
  "com.faacets" %% "qalg" % "0.9.2-SNAPSHOT",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

initialCommands in console := """
import com.faacets._; import qalg._; import polyta._
import spire.math.Rational; import qalg.math._

"""
