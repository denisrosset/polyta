package com.faacets.polyta
package solvers

import java.io.{File, FileReader, PrintWriter}

import scala.sys.process._

import spire.math.Rational

import com.faacets.polyta.ComparisonOp.LE
import com.faacets.polyta.formats.{Format, FormatRead, FormatWrite}
import com.faacets.polyta.formats.qsopt.{LPConstraint, LPData, LPObjective, SolData}
import com.faacets.polyta.formats.sympol.{ExtData, SymmetryInfo}
import com.faacets.polyta.process.{Computation, Runner}
import scalin.Vec
import scalin.immutable.dense._
import scalin.syntax.all._

case class LinearSolution(status: SolverStatus[String], optimalValue: Rational, optimalSolution: Vec[Rational])

object QSoptEx {

  trait Files[Input, Output] extends Computation[Input, Output] {

    type InputFile = Some[File]
    type OutputFile = Some[File]

    def inputExtension: String

    def outputAddedExtension: String

    def newInputFile(): InputFile = Some(File.createTempFile("qsoptex", inputExtension))

    def newOutputFile(inputFile: InputFile): OutputFile =
      Some(new File(inputFile.get.getAbsolutePath + outputAddedExtension))

  }

 object Template extends Files[LPData, SolData] {
   def inputExtension = ".lp"
   def outputAddedExtension = ".sol"
   def inputFormat: FormatWrite[LPData] = LPData.formatWrite
   def outputFormat: FormatRead[SolData] = SolData.formatRead
   def commandLine(inputFile: Some[File], outputFile: Some[File]): String =
     "esolver -O " + outputFile.get.getAbsolutePath + " -L " + inputFile.get.getAbsolutePath
 }

  def solve(hPolytope: HPolytope[Rational], direction: Direction = Direction.Min, f: Vec[Rational], integerVariables: Set[Int] = Set.empty[Int]): LinearSolution = {
    val lpData = LPData.fromPolytope(hPolytope, direction, f, integerVariables)
    val (solData, _) = Runner.throwing(Template, lpData)
    LinearSolution(solData.status, solData.optimalValue.getOrElse(0),
      vec(lpData.variableNames.map(solData.variables.getOrElse(_, Rational.zero)):_*))
  }

}
