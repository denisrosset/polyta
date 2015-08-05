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

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._
import qalg.syntax.indup.all._
import qalg.syntax.algos.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.product._

trait ElementBase[V, G] {
  type E <: ElementBase[V, G]
  override def toString = point.toString
  def point: V
  /** Representatives of this element under symmetry. */
  def representatives: Iterable[E]
  /** Subgroup of the polytope symmetry group leaving this element invariant. */
  def symSubgroup: Grp[G]
}

trait VertexBase[V, G] extends ElementBase[V, G] {
  type E <: VertexBase[V, G]
  def representatives: Iterable[E]
}

trait RayBase[V, G] extends ElementBase[V, G] {
  type E <: RayBase[V, G]
  def representatives: Iterable[E]
}

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

trait VPolytope[V, @sp(Double) A] extends Polytope[V, A] { lhs =>
  implicit val pack: PackField.ForV[V, A]
  implicit def orderA: Order[A]
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "\nRays:\n" + rays.mkString("\n")

  type Element <: ElementBase[V, G]
  type Vertex <: Element with VertexBase[V, G] { type E = Vertex }
  type Ray <: Element with RayBase[V, G] { type E = Ray }

  def vertices: Seq[Vertex]
  def allVertices: Seq[Vertex] = vertices.flatMap(_.representatives)
  def rays: Seq[Ray]
  def allRays: Seq[Ray] = rays.flatMap(_.representatives)

  /** Action of the symmetry group on vertices and rays. */
  implicit def elementAction: Action[Element, G]
  implicit def vertexAction: Action[Vertex, G]
  implicit def rayAction: Action[Ray, G]

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
  }
}


object VPolytope {
  type ForG[V, A, G0] = VPolytope[V, A] { type G = G0 }
  def apply[M, V, @sp(Double, Long) A: Order](vertexPoints: M, rayPoints: M)(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A, (Perm, Perm)] =
    VPolytopeCombSym[M, V, A, Perm, Perm](
      vertexPoints, rayPoints, Grp.trivial[(Perm, Perm)])

  def apply[V, @sp(Double, Long) A: Order](nX: Int, vertexPoints: Seq[V], rayPoints: Seq[V])(implicit pack: PackField.ForV[V, A]): VPolytopeCombSym[_, V, A, (Perm, Perm)] = {
    type M = pack.M
    implicit val M = pack.M
    val vertexM = M.fromRows(nX)(vertexPoints: _*)
    val rayM = M.fromRows(nX)(rayPoints:_ *)
    apply(vertexM, rayM)(implicitly, pack)
  }
}
