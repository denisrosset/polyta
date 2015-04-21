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

/** Polyhedron described by extremal rays and vertices, which are stored as
  * row vectors.
  */
trait VPolyhedronZS[M, V, @sp(Double) A] extends VPolyhedron[V, A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def MM: MatMutable[M, A]
  implicit def VM: VecMutable[V, A]
  implicit def orderA: Order[A]
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n")

  def hPolyhedron: HPolyhedron[V, A]
  def vertexZeroSets: IndexedSeq[Set[Int]]

  def vertices: IndexedSeq[V] = new IndexedSeq[V] {
    def length = vertexZeroSets.length
    def apply(k: Int): V = {
      val ineqSatisfied: Seq[(V, A)] = vertexZeroSets(k).toSeq.map {
        i => (hPolyhedron.inequalities(i).lhs, hPolyhedron.inequalities(i).rhs)
      }
      val eqSatisfied: Seq[(V, A)] = hPolyhedron.equalities.map( eq => (eq.lhs, eq.rhs) )
      val satisfied = ineqSatisfied ++ eqSatisfied
      val newA = M.fromRows(nX, satisfied.map(_._1): _*)
      val newb = V.build(satisfied.map(_._2): _*)
      lu(newA).solve(newb)
    }
  }
  // rays are not yet supported
  def rays: IndexedSeq[V] = IndexedSeq.empty[V]

  def nX: Int = hPolyhedron.nX
}

object VPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](hPolyhedron0: HPolyhedron[V, A], vertexZeroSets0: IndexedSeq[Set[Int]])(implicit M0: MatVecInField[M, V, A], MM0: MatMutable[M, A], VM0: VecMutable[V, A], orderA0: Order[A]): VPolyhedronZS[M, V, A] = new VPolyhedronZS[M, V, A] {
    def M = M0
    def MM = MM0
    def VM = VM0
    def V = M0.V
    def orderA = orderA0
    def hPolyhedron = hPolyhedron0
    def vertexZeroSets = vertexZeroSets0
  }

  def apply[M, V, @sp(Double, Long) A](hPolyhedron: HPolyhedron[V, A], vertexZeroSets: IndexedSeq[Set[Int]])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], VM: VecMutable[V, A], orderA: Order[A]): VPolyhedronZS[M, V, A] = build(hPolyhedron, vertexZeroSets)
}
