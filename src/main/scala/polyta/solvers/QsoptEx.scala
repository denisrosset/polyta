package com.faacets
package polyta
package solvers

import Predef.{any2stringadd => _,_}


import scala.{specialized => sp}
import java.io.{File, PrintWriter, FileReader}

import spire.algebra.Order
import spire.math.Rational
import spire.std.tuples._
import spire.syntax.group._
import spire.util.Opt

import qalg.algebra._
import qalg.algos._

import net.alasc.algebra._
import net.alasc.math.{Grp, Perm}
import net.alasc.std.any._
import net.alasc.syntax.shiftablePermutation._

import formats._
import formats.qsopt._

import sys.process._

case class LinearSolution[V](status: SolverStatus[String], fval: Rational, xopt: V)
object QsoptEx {
  def solve[V, G](hPolytope: HPolytope[V, Rational], direction: Direction, f: V, integerVariables: Set[Int])(implicit pack: PackField.ForV[V, Rational]): LinearSolution[V] = {
    val input = File.createTempFile("solvelp", ".lp")
    val output = File.createTempFile("solvelp", ".sol")
    val writer = new PrintWriter(input)
    val ineqs = hPolytope.allFacets.zipWithIndex.map { case (facet, i) => LPConstraint("ineq" + (i + 1).toString, facet.inequality) }
    val eqs = hPolytope.equalities.zipWithIndex.map { case (equality, i) => LPConstraint("eq" + (i + 1).toString, equality) }
    val variableNames = Format.x1toN(hPolytope.nX)
    val lpData = LPData("linearProblem",
      variableNames,
      LPObjective("obj", direction, f),
      ineqs ++ eqs,
      Box.unbounded[V, Rational](hPolytope.nX),
      integerVariables)
    FormatWrite[LPData[V]].write(lpData, writer)
    writer.close
    val out = new StringBuilder
    val err = new StringBuilder

    val logger = ProcessLogger(
      (o: String) => out.append(o),
      (e: String) => err.append(e))

    ("esolver -O " + output.getAbsolutePath + " -L " + input.getAbsolutePath) ! logger

    val reader = new FileReader(output)
    val data = FormatRead[SolData].parse(reader).get
    reader.close
    LinearSolution[V](data.status, data.optimalValue.getOrElse(0),
      VecBuild[V, Rational].build(variableNames.map(data.variables.getOrElse(_, Rational.zero)):_*))
  }
}
