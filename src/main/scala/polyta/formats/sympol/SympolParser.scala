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

trait SympolParser[M, V] extends RationalParser with AgnosticLineEndingParser with ParserUtils {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  override val whiteSpace = """([ \t])+""".r

  def upToSymBeginLE: Parser[Boolean] =
    opt("* UP TO SYMMETRY" ~ lineEnding) <~ ("begin" ~ lineEnding) ^^ {
      case Some(text) => true
      case None => false
    }

  def dimensions: Parser[(Int, Int)] =
    positiveInt ~ positiveIntShift1 <~ (("integer" | "rational") ~ lineEnding) ^^ {
      case m ~ d => (m, d)
    }

  def rowVector(nC: Int): Parser[V] = repN(nC, rational) ^^ { cols => V.build(cols: _*) }

  def matrix(nR: Int, nC: Int): Parser[M] = repN(nR, rowVector(nC) <~ lineEnding) into { rows =>
    reportException(M.vertcat(rows.map(_.rowMat[M]): _*))
  }

  val lineRegex = """[^\n]*""".r
  def commentLine = not("V-representation" | "H-representation") ~ lineRegex
  def comments = rep(commentLine ~ lineEnding)

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
