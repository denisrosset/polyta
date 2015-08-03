package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
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
import net.alasc.std.unit._

trait ElementBase[V] extends Any {
  def point: V
  def representatives: Iterable[ElementBase[V]]
}

trait VertexBase[V] extends Any with ElementBase[V] {
  def representatives: Iterable[VertexBase[V]]
}

trait RayBase[V] extends Any with ElementBase[V] {
  def representatives: Iterable[RayBase[V]]
}

final class SingleVertex[V](val point: V) extends AnyVal with VertexBase[V] {
  def representatives = Iterable(this)
}

final class SingleRay[V](val point: V) extends AnyVal with RayBase[V] {
  def representatives = Iterable(this)
}

/** Polytope described by extremal vertices. */

trait VPolytope[V, @sp(Double) A] extends Polytope[V, A] { lhs =>
  implicit val pack: PackField.ForV[V, A]
  implicit def orderA: Order[A]
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n")

  type Element <: ElementBase[V]
  type Vertex <: Element with VertexBase[V]
  type Ray <: Element with RayBase[V]

  def vertices: Seq[Vertex]
  def rays: Seq[Ray]

  /** Action of the symmetry group on vertices and rays. */
  implicit def elementAction: Action[Element, G]
  implicit def vertexAction: Action[Vertex, G]
  implicit def rayAction: Action[Ray, G]

  /** Subgroup of `symGroup` leaving the vertex `v` invariant. */
  def symSubgroup(e: Element): Grp[G]

  /** TODO: what happens with rays ? */
  def facetOn(vertexIndices: Set[Int]): LinearInequality[V, A] = {
    implicit def A: Field[A] = pack.A
    val zeroSeq = vertexIndices.toSeq
    val zeroH = vertices(zeroSeq.head).point
    val zeroT = zeroSeq.tail
    val zeroVertices = zeroT.toSeq.map(vertices(_).point - zeroH)
    val nonZeroIndex = (vertices.indices.toSet -- vertexIndices).head
    val nonZeroVertex = vertices(nonZeroIndex).point - zeroH
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
      val subgrp = symSubgroup(vertex)
      val cosets = subgrp \ symGroup
      cosets.iterator.map { coset => (vertex <|+| coset.g).point }
    }
    val rayPoints: Seq[V] = rays.flatMap { ray =>
      val subgrp = symSubgroup(ray)
      val cosets = subgrp \ symGroup
      cosets.iterator.map { coset => (ray <|+| coset.g).point }
    }
    VPolytope(nX, vertexPoints, rayPoints)
  }
}

// TODO: move to matrix storage
final class VPolytopeNoSym[M, V, @sp(Double, Long) A](val vertexPoints: M, val rayPoints: M)(implicit val pack: PackField.ForMV[M, V, A], val orderA: Order[A]) extends VPolytope[V, A] {
  require(vertexPoints.nCols == rayPoints.nCols)
  def nX = vertexPoints.nCols
  def vertices: Seq[Vertex] = new IndexedSeq[Vertex] {
    def length = vertexPoints.nRows
    def apply(i: Int): Vertex = new SingleVertex(vertexPoints(i, ::))
  }
  def rays: Seq[Ray] = new IndexedSeq[Ray] {
    def length = rayPoints.nCols
    def apply(i: Int): Ray = new SingleRay(rayPoints(i, ::))
  }
  object elementAction extends Action[Element, Unit] {
    def actl(g: Unit, e: Element): Element = e
    def actr(e: Element, g: Unit): Element = e
  }
  object vertexAction extends Action[Vertex, Unit] {
    def actl(g: Unit, v: Vertex): Vertex = v
    def actr(v: Vertex, g: Unit): Vertex = v
  }
  object rayAction extends Action[Ray, Unit] {
    def actl(g: Unit, r: Ray): Ray = r
    def actr(r: Ray, g: Unit): Ray = r
  }
  type Element = ElementBase[V]
  type Vertex = SingleVertex[V]
  type Ray = SingleRay[V]
  type G = Unit
  val symGroup: Grp[Unit] = Grp(())
  def symSubgroup(e: Element): Grp[G] = Grp(())
  override def flatten = this
}

object VPolytope {
  def apply[M, V, @sp(Double, Long) A: Order](vertexPoints: M, rayPoints: M)(implicit pack: PackField.ForMV[M, V, A]): VPolytopeNoSym[M, V, A] = new VPolytopeNoSym[M, V, A](vertexPoints, rayPoints)
  def apply[V, @sp(Double, Long) A: Order](nX: Int, vertexPoints: Seq[V], rayPoints: Seq[V])(implicit pack: PackField.ForV[V, A]): VPolytope[V, A] = {
    type M = pack.M
    implicit val M = pack.M
    val vertexM = M.fromRows(nX)(vertexPoints: _*)
    val rayM = M.fromRows(nX)(rayPoints:_ *)
    apply(vertexM, rayM)(implicitly, pack)
  }
}
