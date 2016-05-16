package com.faacets
package polyta
package formats
package sympol

import spire.algebra._
import spire.math.{Rational, SafeLong}

import fastparse.WhitespaceApi

import net.alasc.perms.{Cycle, Perm}
import net.alasc.syntax.all._
import scalin.Vec
import scalin.immutable.dense._
import scalin.syntax.all._

object Sympol {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \t".contains(_)).?)
  }

}

trait SympolParsers extends RationalParsers with AgnosticLineEndingParsers {

  import fastparse.noApi._

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    Sympol.White.parserApi(p0)(c)

  val line = CharsWhile(_ != '\n')

  def commentLine[U](notLine: P[U]) = !notLine ~ line

  def comments[U](notLine: P[U]) = (commentLine(notLine) ~ lineEnding).rep

  def positiveIntShift1: P[Int] = positiveInt.map(_ - 1)

  def cycle: P[Perm] = positiveIntShift1.rep(min=2).map( cycle => Cycle(cycle: _*).toPermutation[Perm] )

  def cycles: P[Perm] = cycle.rep(sep=",").map( cycles => Group[Perm].combine(cycles) )

  def generators: P[Seq[Perm]] = P( nonNegativeInt ~ lineEnding ).flatMap { nGenerators =>
    (Pass ~ cycles ~ lineEnding).rep(min=nGenerators, max=nGenerators)
  }

  def base: P[Seq[Int]] = P( nonNegativeInt ~ lineEnding ).flatMap { nBaseElements =>
    if (nBaseElements == 0)
      Pass.map( x =>  Seq.empty )
    else
      Pass ~ positiveIntShift1.rep(min=nBaseElements, max=nBaseElements) ~ lineEnding
  }

  val orderAndCommentOption =
    P( "* order " ~/ positiveSafeLong ~ lineEnding ).?

  val orderAndCommentWRT =
    P( orderAndCommentOption ~/ "* w.r.t. to the original inequalities/vertices" ~ lineEnding )

  def orderAndComment(upToSymmetry: Boolean): P[Option[SafeLong]] =
    if (upToSymmetry) orderAndCommentWRT else orderAndCommentOption

  def symmetryInfo(upToSymmetry: Boolean): P[SymmetryInfo] =
    P( "permutation group" ~ lineEnding ~ orderAndComment(upToSymmetry) ~ generators ~ base ).map {
      case (o, g, b) => SymmetryInfo(upToSymmetry, o, g, b)
    }

  def upToSymLE: P[Boolean] =
    P( ("* UP TO SYMMETRY" ~ lineEnding).!.? ).map {
      case Some(_) => true
      case None => false
    }

  def upToSymBeginLE: P[Boolean] =
    P( ("* UP TO SYMMETRY" ~ lineEnding).!.? ~ "begin" ~/ lineEnding ).map {
      case Some(_) => true
      case None => false
    }

  def dimensions: P[(Int, Int)] =
    P( positiveInt ~ positiveIntShift1 ~ ("integer" | "rational") ~ lineEnding ).map {
      case (m, d) => (m, d)
    }

  def rowVector(nC: Int): P[Vec[Rational]] = rational.rep(min=nC, max=nC).map( cols => vec(cols: _*) )

  def HVHeader = "V-representation" | "H-representation"

}
