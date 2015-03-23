package com.faacets
package polyta
package format
package qsopt

import scala.{specialized => sp}

import scala.collection.BitSet

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

case class LPData[M, V, @sp(Double) A: Order](
  name: Option[String],
  objectiveName: String,
  variableNames: Seq[String],
  ineqConstraintNames: Seq[String],
  eqConstraintNames: Seq[String],
  program: MixedLinearProgram[M, V, Rational])
  (implicit
    MV: MatVecInField[M, V, A]) {
  implicit def V: VecInField[V, A] = MV.V
  implicit def A: Field[A] = MV.scalar
  def vectorAsString(v: V): String = {
    val sb = new StringBuilder
    var plusString = ""
    var minusString = "-"
    var space = ""
    cforRange(0 until v.length) { k =>
      val sign = v(k).compare(A.zero)
      if (sign > 0) {
        sb ++= space
        sb ++= plusString
        sb ++= " "
        if (v(k) =!= A.one) sb ++= v(k).toString
        sb ++= variableNames(k)
        plusString = "+"
        space = " "
      } else if (sign < 0) {
        sb ++= space
        sb ++= minusString
        sb ++= " "
        if (-v(k) =!= A.one) sb ++= (-v(k)).toString
        sb ++= variableNames(k)
        plusString = "+"
        space = " "
      }
    }
    sb.result
  }
  def problemSection = name match {
    case Some(n) => s"Problem\n   $n\n"
    case None => ""
  }
  def objectiveSection = {
    val firstLine = program.direction match {
      case Max => "Maximize\n"
      case Min => "Minimize\n"
    }
    val objExpr = vectorAsString(program.objective)
    val secondLine = s"   $objectiveName: $objExpr\n"
    firstLine + secondLine
  }
  def constraintSection = {
    val firstLine = "Subject\n"
    val sb = new StringBuilder
    sb ++= firstLine
    cforRange(0 until program.feasibleSet.nEqs) { r =>
      val name = eqConstraintNames(r)
      val expr = vectorAsString(program.feasibleSet.mAeq(r, ::))
      val coeff = program.feasibleSet.vbeq(r)
      sb ++= s"    $name: $expr = $coeff\n"
    }
    cforRange(0 until program.feasibleSet.nIneqs) { r =>
      val name = ineqConstraintNames(r)
      val expr = vectorAsString(program.feasibleSet.mA(r, ::))
      val coeff = program.feasibleSet.vb(r)
      sb ++= s"    $name: $expr <= $coeff\n"
    }
    sb.result
  }
  def integerSection = if (program.integerVariables.isEmpty) "" else
    "Integer\n    " + program.integerVariables.map(variableNames(_)).mkString(" ") + "\n"
  def boundsSection = {
    val firstLine = "Bounds\n"
    val sb = new StringBuilder
    sb ++= firstLine
    cforRange(0 until program.nX) { k =>
      val lb = if (program.bounds.lowerBounds.isBoundSet(k))
        program.bounds.lowerBounds.v(k).toString else "-inf"
      val ub = if (program.bounds.upperBounds.isBoundSet(k))
        program.bounds.upperBounds.v(k).toString else "inf"
      val name = variableNames(k)
      sb ++= s"    $lb <= $name <= $ub\n"
    }
    sb.result
  }
  def endSection = "End\n"
  def content = problemSection + objectiveSection + constraintSection + boundsSection + integerSection + endSection
}
