package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._

/** Generic convex set base class. */
trait ConvexSet[M, V, @sp(Double) A] extends Any {
  implicit def MV: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = MV.V
  implicit def A: Field[A] = MV.scalar
  /** Number of variables. */
  def nX: Int
}

trait LinearConvexSet[M, V, @sp(Double) A] extends Any with ConvexSet[M, V, A]

case class ConvexSetIntersection[M, V, @sp(Double) A](sets: Iterable[ConvexSet[M, V, A]])(implicit val MV: MatVecInField[M, V, A]) extends ConvexSet[M, V, A] {
  require(sets.nonEmpty)
  val nX: Int = sets.head.nX
  require(sets.forall(_.nX == nX))
}

