package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

import fastparse.WhitespaceApi
import scalin.immutable.Vec
import scalin.immutable.dense._
import scalin.syntax.all._


object Panda {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \t".contains(_)).?)
  }

}

/** Base trait for Panda data files.
  * 
  * All sections are parsed without the final line ending.
  */
trait PandaDataParsers extends AgnosticLineEndingParsers with RationalParsers {

  import fastparse.noApi._

  val variable: P[String] =
    P( (CharPred(java.lang.Character.isJavaIdentifierStart(_)) ~~ CharsWhile(java.lang.Character.isJavaIdentifierPart(_)).?).! )

  val dimSection: P[Int] = P( "DIM" ~ "=" ~ positiveInt )

  val namesHeading = P( "Names:" | "INDEX" | "INDICES" | "NAMES" )

  val namesSection: P[Seq[String]] = P( namesHeading ~/ lineEndings ~ variable.rep(min=1) )

  val rowVector: P[Vec[Rational]] = rational.rep(min=1).map( vec(_: _*) )

  def rowVector(dim: Int): Parser[Vec[Rational]] = rational.rep(min=dim, max=dim).map( vec(_: _*) )

  val namedHeader: P[Seq[String]] = P( (dimSection ~ lineEndings).? ~ namesSection ).map {
    case (None, seq) => seq
    case (Some(d), seq) if seq.size == d => seq
    case _ => throw new IllegalArgumentException("Given dimension does not correspond to number of named variables.")
  }

  val unnamedHeader: P[Int] = dimSection

  val mapsHeader = P( "Maps:" | "MAPS" )

  val mapLine = CharsWhile( x => (x != '\n') && (x != '\r') )

  def mapAffineTransform(names: Seq[String]): P[AffineTransform[Rational]] =
    P( AffineTransformParsers.terms(names) ~ lineEnding )

  type Maps = Seq[AffineTransform[Rational]]

  def mapsAffineTransforms(names: Seq[String]): P[Maps] =
    P( mapsHeader ~ (lineEndings ~ mapAffineTransform(names)).rep )


  def mapsSection(namesOption: Option[Seq[String]]): P[Left[Maps, Nothing]] = namesOption match {
    case Some(names) => mapsAffineTransforms(names).map( maps => Left(maps) )
    case None => Fail
  }

  def sectionEnd = P( lineEndings ~ ("END" ~ lineEndings).? )

}
