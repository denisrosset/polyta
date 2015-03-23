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

case class Bounds[V](v: V, isBoundSet: BitSet)

object Bounds {
  def unbounded[V, @sp(Double) A](nX: Int)(implicit V0: VecInField[V, A]): Bounds[V] = {
    import V0.scalar
    Bounds(V0.zeros(nX), BitSet.empty)
  }
  def apply[V](v: V)(implicit V0: VecInField[V, Double]): Bounds[V] = {
    val isBoundSet = collection.mutable.BitSet.empty
    cforRange(0 until v.length) { k =>
      if (!v(k).isInfinity)
        isBoundSet += k
    }
    Bounds(v, isBoundSet)
  }
}
