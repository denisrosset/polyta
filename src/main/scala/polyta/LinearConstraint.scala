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
  override def toString = lhs.toString + " " + op.toString + " " + rhs.toString
  def lhs: V
  def op: ComparisonOperator
  def rhs: A
}

trait LinearEquality[V, @sp(Double, Long) A] extends LinearConstraint[V, A] {
  def op = EQ
}

object LinearEquality {
  final class Impl[V, @sp(Double, Long) A](val lhs: V, val rhs: A) extends LinearEquality[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A): LinearEquality[V, A] = new Impl(lhs, rhs)
}

trait LinearInequality[V, @sp(Double, Long) A] extends LinearConstraint[V, A] { self =>
  def toLE(implicit V: VecRing[V, A], A: Ring[A]): LinearInequality[V, A] = if (op == LE) self else LinearInequality(-lhs, LE, -rhs)
  def toGE(implicit V: VecRing[V, A], A: Ring[A]): LinearInequality[V, A] = if (op == GE) self else LinearInequality(-lhs, GE, -rhs)
}

object LinearInequality {
  final class Impl[V, @sp(Double, Long) A](val lhs: V, val op: InequalityOperator, val rhs: A) extends LinearInequality[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V, op: InequalityOperator, rhs: A): LinearInequality[V, A] = new Impl(lhs, op, rhs)
}

/*
object LinearConstraint {
  def unapply[V, @sp(Double, Long) A](lc: LinearConstraint[V, A]): Option[(V, ComparisonOperator, A)] =
    Some((lc.lhs, lc.op, lc.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, op: ComparisonOperator, rhs: A)(implicit V: VecRing[V, A]): LinearConstraint[V, A] = op match {
    case LE => LinearInequalityLE(lhs, rhs)
    case EQ => LinearEquality(lhs, rhs)
    case GE => LinearInequalityGE(lhs, rhs)
  }
}
 */

/*
object LinearEquality {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecRing[V, A]) extends LinearEquality[V, A]
  def unapply[V, @sp(Double, Long) A](le: LinearEquality[V, A]): Option[(V, A)] = Some((le.lhs, le.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecRing[V, A]): LinearEquality[V, A] = new Instance(lhs, rhs)
}


sealed trait LinearInequalityLE[V, @sp(Double, Long) A] extends LinearInequality[V, A] { self =>
  def op = LE
  def toLE = self
  def toGE = LinearInequalityGE(-lhs, -rhs)
}

object LinearInequalityLE {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecRing[V, A]) extends LinearInequalityLE[V, A]
  def unapply[V, @sp(Double, Long) A](li: LinearInequalityLE[V, A]): Option[(V, A)] = Some((li.lhs, li.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecRing[V, A]): LinearInequalityLE[V, A] = new Instance(lhs, rhs)
}

sealed trait LinearInequalityGE[V, @sp(Double, Long) A] extends LinearInequality[V, A] { self =>
  def op = GE
  def toLE = LinearInequalityLE(-lhs, -rhs)
  def toGE = self
}

object LinearInequalityGE {
  class Instance[V, @sp(Double, Long) A](val lhs: V, val rhs: A)(implicit val V: VecRing[V, A]) extends LinearInequalityGE[V, A]
  def unapply[V, @sp(Double, Long) A](li: LinearInequalityGE[V, A]): Option[(V, A)] = Some((li.lhs, li.rhs))
  def apply[V, @sp(Double, Long) A](lhs: V, rhs: A)(implicit V: VecRing[V, A]): LinearInequalityGE[V, A] = new Instance(lhs, rhs)
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
  class Instance[V, @sp(Double, Long) A](val lhs: V)(implicit val V: VecRing[V, A]) extends LinearInequalityLEHom[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V)(implicit V: VecRing[V, A]): LinearInequalityLEHom[V, A] = new Instance(lhs)
}

sealed trait LinearInequalityGEHom[V, @sp(Double, Long) A] extends LinearInequalityGE[V, A] with LinearInequalityHom[V, A] { self =>
  override def toLE = LinearInequalityLEHom(-lhs)
  override def toGE = self
}

object LinearInequalityGEHom {
  class Instance[V, @sp(Double, Long) A](val lhs: V)(implicit val V: VecRing[V, A]) extends LinearInequalityGEHom[V, A]
  def apply[V, @sp(Double, Long) A](lhs: V)(implicit V: VecRing[V, A]): LinearInequalityGEHom[V, A] = new Instance(lhs)
}
*/
