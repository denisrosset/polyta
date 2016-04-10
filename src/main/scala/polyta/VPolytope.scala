package com.faacets
package polyta

import spire.algebra._
import spire.syntax.innerProductSpace._

import net.alasc.finite._

import scalin.immutable.Vec
import scalin.syntax.all._

/** Polytope described by extremal vertices. */

trait VPolytope[A] extends Polytope[A] { lhs =>

  type Element <: VPolytope.Element.ForG[A, G]
  type Vertex <: Element with VPolytope.Vertex.ForG[A, G] { type E = Vertex }
  type Ray <: Element with VPolytope.Ray.ForG[A, G] { type E = Ray }

  /** Vertex representatives, one for each orbit. */
  def vertices: Seq[Vertex]

  /** All vertices. */
  def allVertices: Seq[Vertex] = vertices.flatMap(_.representatives)

  /** Ray representatives, one for each orbit. */
  def rays: Seq[Ray]

  /** All rays. */
  def allRays: Seq[Ray] = rays.flatMap(_.representatives)

  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "\nRays:\n" + rays.mkString("\n")

  /** Action of the symmetry group on vertices and rays. */
  implicit def elementAction: Action[Element, G]

  /** Action of the symmetry group on vertices. */
  implicit def vertexAction: Action[Vertex, G]

  /** Action of the symmetry group on rays. */
  implicit def rayAction: Action[Ray, G]

  def symmetriesDiscarded: VPolytope[A] = {
    val allVertexPoints: Seq[Vec[A]] = allVertices.map(_.point)
    val allRayPoints: Seq[Vec[A]] = allRays.map(_.point)
    VPolytopeM(dim, allVertexPoints, allRayPoints)
  }

  def equalities: Seq[LinearEquality[A]] = {
    import A.{IMat, IVec, fieldA}
    val headV = vertices.head.point
    val otherV = vertices.tail.map(_.point)
    val basis = IMat.tabulate(otherV.size, dim) {
      (r, c) => otherV(r)(c) - headV(c)
    }.orthogonalized
    val full = (basis vertcat eye(dim)).orthogonalized

    (basis.nRows until full.nRows).map { r =>
      val row = full(r, ::)

      LinearEquality(row, row.dot(headV))
    }
  }

}


object VPolytope {

  type ForG[A, G0] = VPolytope[A] { type G = G0 }

  trait Element[A] {
    def point: Vec[A]
    override def toString = point.toString
    /** Symmetry group element type. */
    type G
    /** Self type. */
    type E <: Element.ForG[A, G]
    /** Representatives of this element under symmetry. */
    def representatives: Iterable[E]
    /** Subgroup of the polytope symmetry group leaving this element invariant. */
    def symSubgroup: Grp[G]
  }

  object Element {
    type ForG[A, G0] = Element[A] { type G = G0 }
  }


  trait Vertex[A] extends Element[A] {
    type E <: Vertex.ForG[A, G]
  }

  object Vertex {
    type ForG[A, G0] = Vertex[A] { type G = G0 }
  }

  trait Ray[A] extends Element[A] {
    type E <: Ray.ForG[A, G]
  }

  object Ray {
    type ForG[A, G0] = Ray[A] { type G = G0 }
  }

}
