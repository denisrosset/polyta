package com.faacets
package polyta

import spire.algebra.Ring
import spire.syntax.field._

import scalin.immutable.Vec

import ComparisonOp.{LE, EQ, GE}

/** A linear inequality or equality constraint, described by a vector of type `V` composed
  * of scalars of type `A`.
  */
sealed trait LinearConstraint[A] {
  override def toString = lhs.toString + " " + op.toString + " " + rhs.toString
  def lhs: Vec[A]
  def op: ComparisonOp
  def rhs: A
}

object LinearConstraint {

  def apply[A](lhs: Vec[A], op: ComparisonOp, rhs: A) = op match {
    case EQ => LinearEquality[A](lhs, rhs)
    case iop: InequalityOp => LinearInequality[A](lhs, iop, rhs)
  }

  def unapply[A](lc: LinearConstraint[A]): Option[(Vec[A], ComparisonOp, A)] =
    Some((lc.lhs, lc.op, lc.rhs))

}

case class LinearEquality[A](lhs: Vec[A], rhs: A) extends LinearConstraint[A] {
  def op = EQ
}

case class LinearInequality[A](lhs: Vec[A], op: InequalityOp, rhs: A) extends LinearConstraint[A] {

  def toLE(implicit la: LinAlg[A]): LinearInequality[A] = if (op == LE) this else LinearInequality(la.IVec.negate(lhs), LE, la.fieldA.negate(rhs))
  def toGE(implicit la: LinAlg[A]): LinearInequality[A] = if (op == GE) this else LinearInequality(la.IVec.negate(lhs), GE, la.fieldA.negate(rhs))

}
