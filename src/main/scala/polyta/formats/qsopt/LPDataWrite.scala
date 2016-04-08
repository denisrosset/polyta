package com.faacets
package polyta
package formats
package qsopt

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import SolverStatus._
import Direction._

class LPDataWrite extends FormatWrite[LPData] {

  def ident = "    "

  def writeProblem(problemName: String, out: Writer): Unit = {
    out.write("Problem\n")
    out.write(ident)
    out.write(problemName)
    out.write("\n")
  }

  def writeObjective(objective: LPObjective, variableNames: Seq[String], out: Writer): Unit  = {
    val firstLine = objective.direction match {
      case Max => out.write("Maximize\n")
      case Min => out.write("Minimize\n")
    }
    out.write(ident)
    out.write(objective.name)
    out.write(":")
    Format.writeVector[Rational](objective.f, variableNames, out)
    out.write("\n")
  }

  def writeConstraint(name: String, constraint: LinearConstraint[Rational], variableNames: Seq[String], out: Writer): Unit = {
    out.write(ident)
    out.write(name)
    out.write(":")
    Format.writeVector[Rational](constraint.lhs, variableNames, out)
    out.write(" ")
    out.write(constraint.op.toString)
    out.write(" ")
    out.write(constraint.rhs.toString)
    out.write("\n")
  }

  def writeConstraints(constraints: Seq[LPConstraint], variableNames: Seq[String], out: Writer): Unit = {
    out.write("Subject\n")
    constraints.foreach { c =>
      writeConstraint(c.name, c.constraint, variableNames, out)
    }
  }

  def writeIntegerVariables(integerVariables: Set[Int], variableNames: Seq[String], out: Writer) = {
    if (integerVariables.nonEmpty) {
      out.write("Integer\n")
      out.write(ident)
      integerVariables.foreach { variable =>
        out.write(variableNames(variable))
        out.write(" ")
      }
      out.write("\n")
    }
  }

  def writeEnd(out: Writer) = out.write("End\n")

  def write(data: LPData, out: Writer): Unit = {
    writeProblem(data.problemName, out)
    writeObjective(data.objective, data.variableNames, out)
    writeConstraints(data.constraints, data.variableNames, out)
    writeIntegerVariables(data.integerVariables, data.variableNames, out)
    writeEnd(out)
  }

}
