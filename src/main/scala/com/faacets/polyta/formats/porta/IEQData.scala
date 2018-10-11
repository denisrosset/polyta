package com.faacets.polyta
package formats
package porta

import java.io.Writer

import com.faacets.polyta.ComparisonOp.{EQ, GE, LE}
import fastparse.noApi._
import spire.math.Rational
import scalin.immutable.dense._
import scalin.immutable.{Mat, Vec}
import spire.syntax.cfor.cforRange

case class IEQData(
  polytope: HPolytope[Rational],
  validPoint: Vec[Rational],
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[Vec[Rational]] = None,
  upperBounds: Option[Vec[Rational]] = None
) {

  def fileContents: String = {
    val sw = new java.io.StringWriter
    IEQData.Write.write(this, sw)
    sw.toString
  }
}

object IEQData {

  def parser: P[IEQData] = Read.parser

  object Read {

    import White._
    import StandardParsers._
    import PortaDataParsers._

    sealed trait Section

    case class LowerBound(lb: Vec[Rational]) extends Section

    case class UpperBound(ub: Vec[Rational]) extends Section

    case class EliminationOrder(eo: Seq[Int]) extends Section

    case class Inequalities(poly: HPolytope[Rational]) extends Section

    case class ValidPoint(x: Vec[Rational]) extends Section

    val variable: P[Int] = P( "x" ~ positiveInt ).map(_ - 1)

    def symbolicVector(d: Int): P[Vec[Rational]] =
      P( rationalCoefficientSignOptional ~ variable ~ (signedRationalCoefficient ~ variable).rep ).map {
        case (coeff0, xInd0, seq) =>
          val varMap: Map[Int, Rational] = (Map(xInd0 -> coeff0) /: seq) {
            case (m, (coeff, xInd)) => m + (xInd -> (m.getOrElse(xInd, Rational.zero) + coeff))
          }
          Vec.tabulate(d)( k => varMap.getOrElse(k, Rational.zero) )
      }

    val operator: P[ComparisonOp] = P("<=").map(x => LE) | P("==").map(x => EQ) | P(">=").map(x => GE)

    def constraint(d: Int): Parser[LinearConstraint[Rational]] =
      P( lineNumberForget.? ~ symbolicVector(d) ~ operator ~ rational ).map {
        case (lhs, op, rhs) => LinearConstraint(lhs, op, rhs)
      }

    def inequalitiesSection(d: Int): P[Inequalities] =
      P( "INEQUALITIES_SECTION" ~/ lineEndings ~ constraint(d).rep(sep = lineEndings) ).map {
        constraints =>
          val eqs = constraints.collect {
            case lc: LinearEquality[Rational] => lc
          }
          val ineqs = constraints.collect {
            case lc: LinearInequality[Rational] => lc
          }
          val mA = Mat.tabulate(ineqs.size, d)( (r, c) =>
            if (ineqs(r).op == LE) ineqs(r).lhs(c) else -ineqs(r).lhs(c)
          )
          val vb = Vec.tabulate(ineqs.size)( i => if (ineqs(i).op == LE) ineqs(i).rhs else -ineqs(i).rhs )
          val mAeq = Mat.tabulate(eqs.size, d)( (r, c) => eqs(r).lhs(c) )
          val vbeq = Vec.tabulate(eqs.size)( i => eqs(i).rhs )
          Inequalities(HPolytope(mA, vb, mAeq, vbeq))
      }

    def validSection(d: Int): P[ValidPoint] =
      P( "VALID" ~/ lineEndings ~ rowVector(d) ).map {
        v => ValidPoint(v)
      }


    def lowerBoundSection(d: Int): P[LowerBound] =
      P( "LOWER_BOUNDS" ~/ lineEndings ~ rowVector(d) ).map {
        lb => LowerBound(lb)
      }

    def upperBoundSection(d: Int): P[UpperBound] =
      P( "UPPER_BOUNDS" ~ lineEndings ~ rowVector(d) ).map {
        ub => UpperBound(ub)
      }

    def eliminationOrderSection(d: Int): P[EliminationOrder] =
      P( "ELIMINATION_ORDER" ~ lineEndings ~ nonNegativeInt.rep ).filter(_.size == d).map { orderSeq =>
        val eliminationOrder = orderSeq.zipWithIndex.filterNot(_._1 == 0).sortBy(_._1).map(_._2)
        EliminationOrder(eliminationOrder)
      }

    def section(d: Int): P[Section] =
      inequalitiesSection(d) | validSection(d) | lowerBoundSection(d) | upperBoundSection(d) | eliminationOrderSection(d)

    def sections(d: Int): P[IEQData] = P( section(d) ~ lineEndings ).rep(min = 1)
      .filter( _.count(_.isInstanceOf[LowerBound]) <= 1 )
      .filter( _.count(_.isInstanceOf[UpperBound]) <= 1 )
      .filter( _.count(_.isInstanceOf[ValidPoint]) <= 1 )
      .filter( _.count(_.isInstanceOf[EliminationOrder]) <= 1)
      .filter( _.count(_.isInstanceOf[Inequalities]) <= 1 )
      .map { sections =>
        val lowerBounds = sections.collect { case sec: LowerBound => sec.lb }.headOption
        val upperBounds = sections.collect { case sec: UpperBound => sec.ub }.headOption
        val validPoint = sections.collect { case sec: ValidPoint => sec.x }.headOption.getOrElse(Vec.zeros[Rational](d))
        val polytope = sections.collect { case sec: Inequalities => sec.poly }.headOption.getOrElse(HPolytope.full[Rational](d))
        val eliminationOrder = sections.collect { case sec: EliminationOrder => sec.eo }.headOption
        IEQData(polytope, validPoint, eliminationOrder, lowerBounds, upperBounds)
      }

    def parser: Parser[IEQData] = P( dimSection ~ lineEndings ).flatMap { d => sections(d) ~ end }

  }

  object Write {

    def writeDim(d: Int, out: Writer): Unit = {
      out.write("DIM = ")
      out.write(d.toString)
      out.write("\n\n")
    }

    def writeValid(valid: Vec[Rational], out: Writer): Unit = {
      out.write("VALID\n")
      StandardWriters.writeVectorSep[Rational](valid, " ", out)
      out.write("\n")
    }

    def writeLowerBounds(lowerBounds: Vec[Rational], out: Writer): Unit = {
      out.write("LOWER_BOUNDS\n")
      StandardWriters.writeVectorSep[Rational](lowerBounds, " ", out)
      out.write("\n")
    }

    def writeUpperBounds(upperBounds: Vec[Rational], out: Writer): Unit = {
      out.write("UPPER_BOUNDS\n")
      StandardWriters.writeVectorSep[Rational](upperBounds, " ", out)
      out.write("\n")
    }

    def writeEliminationOrder(d: Int, eliminationOrder: Seq[Int], out: Writer): Unit = {
      val indices = (eliminationOrder zip (1 to eliminationOrder.size)).toMap
      var prefix = ""
      out.write("ELIMINATION_ORDER\n")
      cforRange(0 until d) { k =>
        out.write(prefix)
        out.write(indices.getOrElse(k, 0).toString)
        prefix = " "
      }
      out.write("\n")
    }

    def writePolytope(dim: Int, poly: HPolytope[Rational], out: Writer): Unit = {

      out.write("INEQUALITIES_SECTION\n")
      val names = StandardWriters.x1toN(dim)

      cforRange(0 until poly.mA.nRows) { r =>
        StandardWriters.writeVector[Rational](poly.mA(r, ::), names, out)
        out.write(" <= ")
        out.write(poly.vb(r).toString)
        out.write("\n")
      }

      cforRange(0 until poly.mAeq.nRows) { r =>
        StandardWriters.writeVector[Rational](poly.mAeq(r, ::), names, out)
        out.write(" == ")
        out.write(poly.vbeq(r).toString)
        out.write("\n")
      }

      out.write("\n")
    }

    def writeEnd(out: Writer): Unit =
      out.write("END\n")

    def write(data: IEQData, out: Writer): Unit = {
      val dim = data.polytope.dim
      writeDim(dim, out)
      if (data.validPoint.toIndexedSeq.exists(!_.isZero))
        writeValid(data.validPoint, out)
      data.lowerBounds.foreach { writeLowerBounds(_, out) }
      data.upperBounds.foreach { writeUpperBounds(_, out) }
      data.eliminationOrder.foreach { writeEliminationOrder(dim, _, out) }
      writePolytope(dim, data.polytope, out)
      writeEnd(out)
    }

  }
}
