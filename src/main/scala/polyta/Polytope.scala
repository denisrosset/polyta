package com.faacets
package polyta

import net.alasc.attributes._
import net.alasc.finite.Grp

/** Polytope. */
trait Polytope[A] extends ConvexSet[A] with Attributable { lhs =>

  implicit val A: LinAlg[A]

  /** Type of the symmetry group elements. */
  type G

  /** (Partial) symmetry group of the polytope. */
  def symGroup: Grp[G]

  /** Generate the full description of the polytope, without taking symmetries in account. */
//  def symmetriesDiscarded: Polytope[A]

}

object Polytope {

  type ForG[A, G0] = Polytope[A] { type G = G0 }

  /** Attribute: the polytope is bounded. */ 
  val bounded = Attr.Bool("bounded")

  /** Attribute: the polytope is full dimensional. */
  val fullDimensional = Attr.Bool("fullDimensional")

  /** Attribute: the polytope contains the origin. */
  val containsOrigin = Attr.Bool("containsOrigin")

}
