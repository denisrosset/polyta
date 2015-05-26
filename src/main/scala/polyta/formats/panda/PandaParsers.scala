package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import scala.util.parsing.combinator._

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import net.alasc.math.Perm

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

trait PandaDataParsersBase extends RationalParsers with JavaTokenParsers {
  def variable: Parser[String] = ident
}

/** Base trait for Panda data files.
  * 
  * All sections are parsed without the final line ending.
  */
trait PandaDataParsers[V] extends PandaDataParsersBase with AgnosticLineEndingParsers with ParsersUtils {
  implicit def V: VecInField[V, Rational]

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def namesHeading = "Names:" | "INDEX" | "INDICES" | "NAMES"

  def namesSection: Parser[Seq[String]] = ((namesHeading ~ lineEndings) ~> rep1(variable))

  def rowVector: Parser[V] = rep1(rational) ^^ { V.build(_: _*) }

  def rowVector(dim: Int): Parser[V] = repN(dim, rational) ^^ { V.build(_: _*) }

  def namedHeader: Parser[Seq[String]] = opt(dimSection <~ lineEndings) ~ namesSection into {
    case None ~ seq => success(seq)
    case Some(d) ~ seq if seq.size == d => success(seq)
    case _ => failure("Given dimension does not correspond to number of named variables.")
  }

  def unnamedHeader: Parser[Int] = dimSection

  def mapsHeader = "Maps:" | "MAPS"

  val mapLine: Parser[String] = """[^\n\r]+""".r

  def mapAffineTransform[M, V](names: Seq[String], atp: AffineTransformParsers[M, V])(implicit M: MatVecInField[M, V, Rational]): Parser[AffineTransform[M, V, Rational]] = mapLine into { line =>
    atp.parseAll(atp.terms(names), line) match {
      case error: atp.NoSuccess => failure(error.msg)
      case atp.Success(result, _) => success(result)
    }
  }

  def mapsAffineTransforms[M, V](names: Seq[String], atp: AffineTransformParsers[M, V])(implicit M: MatVecInField[M, V, Rational]): Parser[Maps[M, V]] = mapsHeader ~> rep(lineEndings ~> mapAffineTransform(names, atp))

  type Maps[M, V] = Seq[AffineTransform[M, V, Rational]]

  def mapsSection[M, V](namesOption: Option[Seq[String]])(implicit M: MatVecInField[M, V, Rational]): Parser[Left[Maps[M, V], Nothing]] = namesOption match {
    case Some(names) =>
      val atp = new AffineTransformParsers[M, V]
      mapsAffineTransforms(names, atp) ^^ { maps => Left(maps) }
    case None => failure("Maps are available for named variables only")
  }

  def sectionEnd = lineEndings ~ opt("END" ~ lineEndings)
}
