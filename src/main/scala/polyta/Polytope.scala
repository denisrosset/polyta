package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

/** Polytope. Unbounded polytopes will be supported in the future. */

trait Polytope[V, @sp(Double, Long) A] extends LinearConvexSet[V, A] { lhs =>
  type G
  /** (Partial) symmetry group of the polytope. */
  def symGroup: Grp[G]

  /** Generate the full description of the polytope, without taking symmetries in account. */
  def flatten: Polytope[V, A]

  def nX: Int

  /** Certificate that the polytope is bounded. */ 
  class Bounded
  /** Certificate that the polytope is full dimensional. */
  class FullDimensional
  /** Certificate that the polytope contains the origin. */
  class ContainsOrigin
}

object Polytope {
  type ForG[V, A, G0] = Polytope[V, A] { type G = G0 }
}
