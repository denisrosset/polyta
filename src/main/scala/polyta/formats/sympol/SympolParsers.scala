package com.faacets
package polyta
package formats
package sympol

import scala.util.parsing.combinator._

import spire.algebra._
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.group._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.math.{Cycle, Cycles, Perm}
import net.alasc.syntax.all._

trait SympolParsers extends RationalParsers with AgnosticLineEndingParsers with ParsersUtils {

  override val whiteSpace = """([ \t])+""".r

  val lineRegex = """[^\n]*""".r

  def commentLine[U](notLine: Parser[U]) = not(notLine) ~ lineRegex

  def comments[U](notLine: Parser[U]) = rep(commentLine(notLine) ~ lineEnding)

    def positiveIntShift1: Parser[Int] = """[1-9]\d*""".r ^^ { str => str.toInt - 1 }

  def cycle: Parser[Perm] = rep(positiveIntShift1) ^^ { cycle => Cycle(cycle: _*).to[Perm] }

  def cycles: Parser[Perm] = repsep(cycle, ",") ^^ { cycles => Group[Perm].combine(cycles) }

  def generators: Parser[Seq[Perm]] = (nonNegativeInt <~ lineEnding) into { nGenerators =>
    repN(nGenerators, cycles <~ lineEnding)
  }

  def base: Parser[Seq[Int]] = (nonNegativeInt <~ lineEnding) into { nBaseElements =>
    if (nBaseElements == 0)
      success(Seq.empty)
    else
      repN(nBaseElements, positiveIntShift1) <~ lineEnding
  }

  def orderAndComment(upToSymmetry: Boolean): Parser[Option[BigInt]] =
    opt(("* order " ~> positiveBigInt) <~ lineEnding) <~ (
      if (upToSymmetry) ("* w.r.t. to the original inequalities/vertices" ~ lineEnding) else success(())
    )

  def symmetryInfo(upToSymmetry: Boolean): Parser[SymmetryInfo] =
    ("permutation group" ~ lineEnding) ~> orderAndComment(upToSymmetry) ~ generators ~ base ^^ {
      case o ~ g ~ b => SymmetryInfo(upToSymmetry, o, g, b)
    }
}

trait SympolParsersV[V] extends SympolParsers {
  implicit def pack: PackField.ForV[V, Rational]

  def upToSymBeginLE: Parser[Boolean] =
    opt("* UP TO SYMMETRY" ~ lineEnding) <~ ("begin" ~ lineEnding) ^^ {
      case Some(text) => true
      case None => false
    }

  def dimensions: Parser[(Int, Int)] =
    positiveInt ~ positiveIntShift1 <~ (("integer" | "rational") ~ lineEnding) ^^ {
      case m ~ d => (m, d)
    }

  def rowVector(nC: Int): Parser[V] = repN(nC, rational) ^^ { cols => VecBuild[V, Rational].build(cols: _*) }

  def HVHeader = "V-representation" | "H-representation"
}
