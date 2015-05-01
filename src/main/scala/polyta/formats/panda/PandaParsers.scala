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

  def mapPerm(names: Seq[String]): Parser[Perm] = rep1(ident) into { seq =>
    val images = seq.map(names.indexOf(_))
    if (images.contains(-1)) failure("Invalid map: only permutations are supported") else success(Perm.fromImages(images))
  }

  def mapsPerms(names: Seq[String]): Parser[Maps] = mapsHeader ~> rep(lineEndings ~> mapPerm(names))

  type Maps = Seq[Perm]

  def mapsSection(namesOption: Option[Seq[String]]): Parser[Left[Maps, Nothing]] = namesOption match {
    case Some(names) => mapsPerms(names) ^^ { maps => Left(maps) }
    case None => failure("Maps are available for named variables only")
  }

  def sectionEnd = lineEndings ~ opt("END" ~ lineEndings)
}
