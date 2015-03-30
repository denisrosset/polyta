name := "Polyta"

organization := "com.faacets"

version := "0.9.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.spire-math" %% "spire" % "0.9.1",
  "net.alasc" %% "alasc" % "0.9.1-SNAPSHOT",
  "net.alasc" %% "alasc-qalg-binding" % "0.9.1-SNAPSHOT",
  "com.faacets" %% "qalg" % "0.9.1-SNAPSHOT",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

initialCommands in console := """
import com.faacets._; import qalg._; import polyta._
import spire.math.Rational; import qalg.math._

"""

