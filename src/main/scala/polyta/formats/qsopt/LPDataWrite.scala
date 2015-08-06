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

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

class LPDataWrite[V](implicit val pack: PackField.ForV[V, Rational]) extends FormatWrite[LPData[V]] {

  def ident = "    "

  def writeProblem(problemName: String, out: Writer): Unit = {
    out.write("Problem\n")
    out.write(ident)
    out.write(problemName)
    out.write("\n")
  }

  def writeObjective(objective: LPObjective[V], variableNames: Seq[String], out: Writer): Unit  = {
    val firstLine = objective.direction match {
      case Max => out.write("Maximize\n")
      case Min => out.write("Minimize\n")
    }
    out.write(ident)
    out.write(objective.name)
    out.write(":")
    Format.writeVector[V, Rational](objective.f, variableNames, out)
    out.write("\n")
  }

  def writeConstraint(name: String, constraint: LinearConstraint[V, Rational], variableNames: Seq[String], out: Writer): Unit = {
    out.write(ident)
    out.write(name)
    out.write(":")
    Format.writeVector[V, Rational](constraint.lhs, variableNames, out)
    out.write(" ")
    out.write(constraint.op.toString)
    out.write(" ")
    out.write(constraint.rhs.toString)
    out.write("\n")
  }

  def writeConstraints(constraints: Seq[LPConstraint[V]], variableNames: Seq[String], out: Writer): Unit = {
    out.write("Subject\n")
    constraints.foreach { c =>
      writeConstraint(c.name, c.constraint, variableNames, out)
    }
  }

  def writeBounds(bounds: Box[V, Rational], variableNames: Seq[String], out: Writer): Unit = {
/*    out.write("Bounds\n")
    bounds.foreach {
      case FreeBound(variable) =>
        out.write(ident)
        out.write(variable)
        out.write(" free\n")
      case ExplicitBound(variable, None, None) =>
      case ExplicitBound(variable, lb, ub) =>
        out.write(ident)
        lb match {
          case Some(_: MinusInf[A]) => out.write("-inf <= ")
          case Some(Scalar(a)) =>
            out.write(a.toString)
            out.write(" <= ")
          case Some(_: PlusInf[A]) => sys.error("Cannot have +infinity as lower bound")
          case None =>
        }
        out.write(variable)
        ub match {
          case Some(_: MinusInf[A]) =>
            sys.error("Cannot have -infinity as upper bound")
          case Some(Scalar(a)) =>
            out.write(" <= ")
            out.write(a.toString)
          case Some(_: PlusInf[A]) => out.write(" <= inf")
          case None =>
        }
        out.write("\n")
    }*/
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
  def write(data: LPData[V], out: Writer): Unit = {
    writeProblem(data.problemName, out)
    writeObjective(data.objective, data.variableNames, out)
    writeConstraints(data.constraints, data.variableNames, out)
    writeBounds(data.bounds, data.variableNames, out)
    writeIntegerVariables(data.integerVariables, data.variableNames, out)
    writeEnd(out)
  }
}
