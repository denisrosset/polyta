package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

import scalin.immutable.Vec
import scalin.immutable.dense._
import scalin.syntax.all._

/** Base trait for Panda data files.
  * 
  * All sections are parsed without the final line ending.
  */
trait PandaDataParsers extends AgnosticLineEndingParsers with ParsersUtils with RationalParsers {

  def variable: Parser[String] = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def namesHeading = "Names:" | "INDEX" | "INDICES" | "NAMES"

  def namesSection: Parser[Seq[String]] = ((namesHeading ~ lineEndings) ~> rep1(variable))

  def rowVector: Parser[Vec[Rational]] = rep1(rational) ^^ { vec(_: _*) }

  def rowVector(dim: Int): Parser[Vec[Rational]] = repN(dim, rational) ^^ { vec(_: _*) }

  def namedHeader: Parser[Seq[String]] = opt(dimSection <~ lineEndings) ~ namesSection into {
    case None ~ seq => success(seq)
    case Some(d) ~ seq if seq.size == d => success(seq)
    case _ => failure("Given dimension does not correspond to number of named variables.")
  }

  def unnamedHeader: Parser[Int] = dimSection

  def mapsHeader = "Maps:" | "MAPS"

  val mapLine: Parser[String] = """[^\n\r]+""".r

  def mapAffineTransform(names: Seq[String]): Parser[AffineTransform[Rational]] = mapLine into { line =>
    AffineTransformParsers.parseAll(AffineTransformParsers.terms(names), line) match {
      case error: AffineTransformParsers.NoSuccess => failure(error.msg)
      case AffineTransformParsers.Success(result, _) => success(result)
    }
  }

  type Maps = Seq[AffineTransform[Rational]]

  def mapsAffineTransforms(names: Seq[String]): Parser[Maps] =
    mapsHeader ~> rep(lineEndings ~> mapAffineTransform(names))


  def mapsSection(namesOption: Option[Seq[String]]): Parser[Left[Maps, Nothing]] = namesOption match {
    case Some(names) => mapsAffineTransforms(names) ^^ { maps => Left(maps) }
    case None => failure("Maps are available for named variables only")
  }

  def sectionEnd = lineEndings ~ opt("END" ~ lineEndings)

}
