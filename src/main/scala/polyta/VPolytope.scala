package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.std.tuples._
import spire.syntax.action._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import net.alasc.algebra._
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.std.product._

import scalin.immutable.{Mat, Vec}
/*
final class SingleVertex[V](val point: V) extends VertexBase[V] {
  type VX = SingleVertex[V]
  def representatives = Iterable(this)
}

final class SingleRay[V](val point: V) extends RayBase[V] {
  type R = SingleRay[V]
  def representatives = Iterable(this)
}
 */

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
/*
  /** TODO: what happens with rays ? */
  def facetOn(onVertices: Seq[Vertex], satisfying: Vertex): LinearInequality[V, A] = {
    implicit def A: Field[A] = pack.A
    val zeroH = onVertices.head.point
    val zeroT = onVertices.tail.map(_.point)
    val zeroVertices = zeroT.map(_ - zeroH)
    val nonZeroVertex = satisfying.point - zeroH
    val lhs = nonZeroVertex.orthogonalized(zeroVertices: _*)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequality(lhs, LE,  rhs)
    else
      LinearInequality(-lhs, LE, -rhs)
  }

  def equalities: Seq[LinearEquality[V, A]] = {
    val headV = vertices.head.point
    val otherV = vertices.tail.map(_.point)
    val basis = otherV.map(_ - headV).orthogonalComplement(nX)
    basis.map( vec => LinearEquality(vec, vec.dot(headV)) )
  }

  def flatten: VPolytope[V, A] = {
    val vertexPoints: Seq[V] = vertices.flatMap { vertex =>
      val subgrp = vertex.symSubgroup
      val cosets = subgrp \ symGroup
      cosets.iterator.map { coset => (vertex <|+| coset.g).point }
    }
    val rayPoints: Seq[V] = rays.flatMap { ray =>
      val subgrp = ray.symSubgroup
      val cosets = subgrp \ symGroup
      cosets.iterator.map { coset => (ray <|+| coset.g).point }
    }
    VPolytope(nX, vertexPoints, rayPoints)
  }*/
}


object VPolytope {

  type ForG[A, G0] = VPolytope[A] { type G = G0 }

  trait Element[A] {

    def point: Vec[A]

    override def toString = point.toString

    /** Symmetry group element type. */
    type G

    /** Self type */
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

  /*
   def apply[M, V, @sp(Double, Long) A: Order](vertexPoints: M, rayPoints: M)(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A, (Perm, Perm)] =
   VPolytopeCombSym[M, V, A, Perm, Perm](
   vertexPoints, rayPoints, Grp.trivial[(Perm, Perm)])

   def apply[V, @sp(Double, Long) A: Order](nX: Int, vertexPoints: Seq[V], rayPoints: Seq[V])(implicit pack: PackField.ForV[V, A]): VPolytopeCombSym[_, V, A, (Perm, Perm)] = {
   type M = pack.M
   implicit val M = pack.M
   val vertexM = M.fromRows(nX)(vertexPoints: _*)
   val rayM = M.fromRows(nX)(rayPoints:_ *)
   apply(vertexM, rayM)(implicitly, pack)
   }*/
}
