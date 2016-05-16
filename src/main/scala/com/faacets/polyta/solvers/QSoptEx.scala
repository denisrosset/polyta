package com.faacets.polyta.solvers

import java.io.{File, FileReader, PrintWriter}

import scala.sys.process._

import spire.math.Rational

import com.faacets.polyta.SolverStatus
import scalin.Vec
import scalin.immutable.dense._
import scalin.syntax.all._

case class LinearSolution(status: SolverStatus[String], optimalValue: Rational, optimalSolution: Vec[Rational])

object QSoptEx {
/*
  def solve(lp: LinearProgram[Rational]): LinearSolution =
    solve(lp.feasibleSet, lp.direction, lp.objective, Set.empty[Int])

  def solve(hPolytope: HPolytope[Rational], direction: Direction, f: Vec[Rational], integerVariables: Set[Int]): LinearSolution = {
    val input = File.createTempFile("solvelp", ".lp")
    val output = File.createTempFile("solvelp", ".sol")
    val writer = new PrintWriter(input)
    val ineqs = hPolytope.allFacets.zipWithIndex.map { case (facet, i) => LPConstraint("ineq" + (i + 1).toString, facet.inequality) }
    val eqs = hPolytope.equalities.zipWithIndex.map { case (equality, i) => LPConstraint("eq" + (i + 1).toString, equality) }
    val variableNames = Format.x1toN(hPolytope.dim)
    val lpData = LPData("linearProblem",
      variableNames,
      LPObjective("obj", direction, f),
      (ineqs ++ eqs).toSeq,
      integerVariables)
    FormatWrite[LPData].write(lpData, writer)
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
    LinearSolution(data.status, data.optimalValue.getOrElse(0),
      vec(variableNames.map(data.variables.getOrElse(_, Rational.zero)):_*))
  }*/

}
