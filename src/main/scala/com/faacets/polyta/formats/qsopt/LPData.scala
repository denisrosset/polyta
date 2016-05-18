package com.faacets
package polyta
package formats
package qsopt

import java.io.{File, FileReader, PrintWriter}

import scala.sys.process.ProcessLogger

import spire.math.Rational

import com.faacets.polyta.ComparisonOp.LE
import com.faacets.polyta.solvers.LinearSolution
import scalin.Vec
import scalin.syntax.all._
import scalin.immutable.dense._

case class LPObjective(name: String, direction: Direction, f: Vec[Rational])

case class LPConstraint(name: String, constraint: LinearConstraint[Rational])

case class LPData(
  val problemName: String,
  val variableNames: Seq[String],
  val objective: LPObjective,
  val constraints: Seq[LPConstraint],
  val integerVariables: Set[Int]
                 )

object LPData {

  implicit val formatWrite: FormatWrite[LPData] = new LPDataWrite

  def fromPolytope(hPolytope: HPolytope[Rational], direction: Direction, f: Vec[Rational], integerVariables: Set[Int] = Set.empty[Int]): LPData = {
    val ineqs = (0 until hPolytope.mA.nRows).map( i => LPConstraint("ineq" + (i + 1).toString, LinearInequality(hPolytope.mA(i, ::), LE, hPolytope.vb(i))) )
    val eqs = (0 until hPolytope.mAeq.nRows).map( i => LPConstraint("eq" + (i + 1).toString, LinearEquality(hPolytope.mAeq(i, ::), hPolytope.vbeq(i))) )
    val variableNames = Format.x1toN(hPolytope.dim)
    LPData("linearProblem",
      variableNames,
      LPObjective("obj", direction, f),
      ineqs ++ eqs,
      integerVariables)
  }

}
