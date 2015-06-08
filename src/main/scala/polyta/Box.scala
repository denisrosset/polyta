package com.faacets
package polyta

import scala.{specialized => sp}

import scala.collection.BitSet
import scala.reflect.ClassTag

import spire.algebra.Eq
import spire.math.Rational
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

case class Box[V, @sp(Double) A](lowerBounds: Bounds[V], upperBounds: Bounds[V])(implicit val alg: AlgVF[V, A]) extends LinearConvexSet[V, A] {
  require(lowerBounds.v.length == upperBounds.v.length)
  def nX: Int = lowerBounds.v.length
}

object Box {
  def unbounded[V, @sp(Double) A](nX: Int)(implicit alg: AlgVF[V, A]): Box[V, A] =
    Box[V, A](Bounds.unbounded[V](nX), Bounds.unbounded[V](nX))
  // special case for Double vector, TODO: why ?
  def apply[V](lb: V, ub: V)(implicit alg: AlgVF[V, Double]): Box[V, Double] =
    Box[V, Double](Bounds(lb), Bounds(ub))
}
