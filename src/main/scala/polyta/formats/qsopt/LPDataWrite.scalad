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

trait LPDataWrite[@sp(Double) A] extends Any {
  implicit def A: Field[A]
  implicit def orderA: Order[A]

  def ident = "    "

  def writeProblem(problemName: Option[String], out: Writer): Unit =
    problemName match {
      case Some(name) =>
        out.write("Problem\n")
        out.write(ident)
        out.write(name)
        out.write("\n")
      case None =>
    }

  def writeObjective(objective: Objective[A], out: Writer): Unit  = {
    val firstLine = objective.direction match {
      case Max => out.write("Maximize\n")
      case Min => out.write("Minimize\n")
    }
    out.write(ident)
    objective.name match {
      case Some(n) =>
        out.write(n)
        out.write(":")
      case None =>
    }
    Format.writeVector(objective.f, out)
    out.write("\n")
  }
  def writeConstraint(constraints: Seq[Constraint[A]], out: Writer): Unit = {
    out.write("Subject\n")
    constraints.foreach { constraint =>
      out.write(ident)
      constraint.name match {
        case Some(n) =>
          out.write(n)
          out.write(":")
        case None =>
      }
      Format.writeVector(constraint.lhs, out)
      out.write(" ")
      out.write(constraint.op.toString)
      out.write(" ")
      Format.writeVector(constraint.rhs, out)
      out.write("\n")
    }
  }
  def writeBounds(bounds: Seq[Bound[A]], out: Writer): Unit = {
    out.write("Bounds\n")
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
    }
  }
  def writeIntegerVariables(integerVariables: Seq[String], out: Writer) = {
    out.write("Integer\n")
    if (integerVariables.nonEmpty) {
      out.write(ident)
      integerVariables.foreach { variable =>
        out.write(variable)
        out.write(" ")
      }
      out.write("\n")
    }
  }
  def writeEnd(out: Writer) = out.write("End\n")
  def write(data: LPData[A], out: Writer): Unit = {
    writeProblem(data.problemName, out)
    writeObjective(data.objective, out)
    writeConstraint(data.constraints, out)
    writeBounds(data.bounds, out)
    writeIntegerVariables(data.integerVariables, out)
    writeEnd(out)
  }
}
