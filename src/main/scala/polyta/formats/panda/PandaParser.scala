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

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._


trait PandaDataParser[V] extends RationalParser with AgnosticLineEndingParser with JavaTokenParsers {
  implicit def V: VecInField[V, Rational]

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def rowVector(d: Int): Parser[V] = repN(d, rational) ^^ { V.build(_: _*) }

  def variable: Parser[String] = ident

  def mapRow: Parser[Seq[String]] = rep1(variable)

  def mapsSection: Parser[HData] = (("Maps" ~ lineEndings) ~> repsep(mapRow, lineEndings)) ^^ {
    seq => HData(maps = seq)
  }

  def end = "END" ~ opt(lineEndings)
}
