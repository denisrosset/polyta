package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra.Ring
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

/** A linear inequality or equality constraint, described by a vector of type `V` composed
  * of scalars of type `A`.
  */
sealed trait LinearConstraint[V, @sp(Double, Long) A] {
  implicit def V: VecInRing[V, A]
  implicit def A: Ring[A] = V.scalar

  override def toString = lhs.toString + " " + op.toString + " " + rhs.toString
  def lhs: V
  def op: ComparisonOperator
  def rhs: A
}

sealed trait LinearConstraintHom[V, @sp(Double, Long) A] extends LinearConstraint[V, A] {
  def rhs: A = A.zero
}

object LinearConstraint {
  def unapply[V, @sp(Double, Long) A](lc: LinearConstraint[V, A]): Option[(V, ComparisonOperator, A)] =
    Some((lc.lhs, lc.op, lc.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, op: ComparisonOperator, rhs: A)(implicit V: VecInRing[V, A]): LinearConstraint[V, A] = op match {
    case LE => LinearInequalityLE(lhs, rhs)
    case EQ => LinearEquality(lhs, rhs)
    case GE => LinearInequalityGE(lhs, rhs)
  }
}

sealed trait LinearEquality[V, @sp(Double, Long) A] extends LinearConstraint[V, A] {
  def op = EQ
}

object LinearEquality {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecInRing[V, A]) extends LinearEquality[V, A]
  def unapply[V, @sp(Double, Long) A](le: LinearEquality[V, A]): Option[(V, A)] = Some((le.lhs, le.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecInRing[V, A]): LinearEquality[V, A] = new Instance(lhs, rhs)
}

sealed trait LinearInequality[V, @sp(Double, Long) A] extends LinearConstraint[V, A] {
  def toLE: LinearInequalityLE[V, A]
  def toGE: LinearInequalityGE[V, A]
}


sealed trait LinearInequalityLE[V, @sp(Double, Long) A] extends LinearInequality[V, A] { self =>
  def op = LE
  def toLE = self
  def toGE = LinearInequalityGE(-lhs, -rhs)
}

object LinearInequalityLE {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecInRing[V, A]) extends LinearInequalityLE[V, A]
  def unapply[V, @sp(Double, Long) A](li: LinearInequalityLE[V, A]): Option[(V, A)] = Some((li.lhs, li.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecInRing[V, A]): LinearInequalityLE[V, A] = new Instance(lhs, rhs)
}

sealed trait LinearInequalityGE[V, @sp(Double, Long) A] extends LinearInequality[V, A] { self =>
  def op = GE
  def toLE = LinearInequalityLE(-lhs, -rhs)
  def toGE = self
}

object LinearInequalityGE {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecInRing[V, A]) extends LinearInequalityGE[V, A]
  def unapply[V, @sp(Double, Long) A](li: LinearInequalityGE[V, A]): Option[(V, A)] = Some((li.lhs, li.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecInRing[V, A]): LinearInequalityGE[V, A] = new Instance(lhs, rhs)
}

sealed trait LinearEqualityHom[V, @sp(Double, Long) A] extends LinearEquality[V, A] with LinearConstraintHom[V, A]


sealed trait LinearInequalityHom[V, @sp(Double, Long) A] extends LinearInequality[V, A] with LinearConstraintHom[V, A] {
  def toLE: LinearInequalityLEHom[V, A]
  def toGE: LinearInequalityGEHom[V, A]
}

sealed trait LinearInequalityLEHom[V, @sp(Double, Long) A] extends LinearInequalityLE[V, A] with LinearInequalityHom[V, A] { self =>
  override def toLE = self
  override def toGE = LinearInequalityGEHom(-lhs)
}

object LinearInequalityLEHom {
  class Instance[V, @sp(Double, Long) A](val lhs: V)(implicit val V: VecInRing[V, A]) extends LinearInequalityLEHom[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V)(implicit V: VecInRing[V, A]): LinearInequalityLEHom[V, A] = new Instance(lhs)
}

sealed trait LinearInequalityGEHom[V, @sp(Double, Long) A] extends LinearInequalityGE[V, A] with LinearInequalityHom[V, A] { self =>
  override def toLE = LinearInequalityLEHom(-lhs)
  override def toGE = self
}

object LinearInequalityGEHom {
  class Instance[V, @sp(Double, Long) A](val lhs: V)(implicit val V: VecInRing[V, A]) extends LinearInequalityGEHom[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V)(implicit V: VecInRing[V, A]): LinearInequalityGEHom[V, A] = new Instance(lhs)
}
