package com.faacets
package polyta

import scala.{specialized => sp}

import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

/** A linear inequality or equality constraint, described by a vector of type `V` composed
  * of scalars of type `A`.
  */
sealed trait LinearConstraint[V, @sp(Double, Long) A] {
  override def toString = lhs.toString + " " + op.toString + " " + rhs.toString
  def lhs: V
  def op: ComparisonOperator
  def rhs: A
}

object LinearConstraint {
  def unapply[V, @sp(Double, Long) A](lc: LinearConstraint[V, A]): Option[(V, ComparisonOperator, A)] =
    Some((lc.lhs, lc.op, lc.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, op: ComparisonOperator, rhs: A): LinearConstraint[V, A] = op match {
    case LE => LinearInequalityLE(lhs, rhs)
    case EQ => LinearEquality(lhs, rhs)
    case GE => LinearInequalityGE(lhs, rhs)
  }
}

case class LinearEquality[V, @sp(Double, Long) A](lhs: V, rhs: A) extends LinearConstraint[V, A] {
  def op = EQ
}

sealed trait LinearInequality[V, @sp(Double, Long) A] extends LinearConstraint[V, A] {
  def toLinearInequalityLE(implicit V: VecInRing[V, A]): LinearInequalityLE[V, A]
  def toLinearInequalityGE(implicit V: VecInRing[V, A]): LinearInequalityGE[V, A]
}

case class LinearInequalityLE[V, @sp(Double, Long) A](lhs: V, rhs: A) extends LinearInequality[V, A] {
  def op = LE
  def toLinearInequalityLE(implicit V: VecInRing[V, A]) = this
  def toLinearInequalityGE(implicit V: VecInRing[V, A]) = {
    import V.scalar
    LinearInequalityGE(-lhs, -rhs)
  }
}

case class LinearInequalityGE[V, @sp(Double, Long) A](lhs: V, rhs: A) extends LinearInequality[V, A] {
  def op = GE
  def toLinearInequalityLE(implicit V: VecInRing[V, A]) = {
    import V.scalar
    LinearInequalityLE(-lhs, -rhs)
  }
  def toLinearInequalityGE(implicit V: VecInRing[V, A]) = this
}
