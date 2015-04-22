package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
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
  def vertexZeroSets: Seq[Set[Int]]

  def vertices: Seq[V] = new IndexedSeq[V] {
    def length = vertexZeroSets.length
    def apply(k: Int): V = VZeroSet(hPolyhedron, vertexZeroSets(k)).vertex
  }
  // rays are not yet supported
  def rays: Seq[V] = Seq.empty[V]

  def nX: Int = hPolyhedron.nX
}

object VPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](hPolyhedron0: HPolyhedron[V, A], vertexZeroSets0: Seq[Set[Int]])(implicit M0: MatVecInField[M, V, A], MM0: MatMutable[M, A], VM0: VecMutable[V, A], orderA0: Order[A]): VPolyhedronZS[M, V, A] = new VPolyhedronZS[M, V, A] {
    def M = M0
    def MM = MM0
    def VM = VM0
    def V = M0.V
    def orderA = orderA0
    def hPolyhedron = hPolyhedron0
    def vertexZeroSets = vertexZeroSets0
  }

  def apply[M, V, @sp(Double, Long) A](hPolyhedron: HPolyhedron[V, A], vertexZeroSets: Seq[Set[Int]])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], VM: VecMutable[V, A], orderA: Order[A]): VPolyhedronZS[M, V, A] = build(hPolyhedron, vertexZeroSets)

  def fromDualDescription[M, V, @sp(Double, Long) A](vPolyhedron: VPolyhedron[V, A], hPolyhedron: HPolyhedron[V, A])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], VM: VecMutable[V, A], orderA: Order[A]): VPolyhedronZS[M, V, A] = {
    import M.V
    require(vPolyhedron.rays.isEmpty)
    val zeroSets = vPolyhedron.vertices.map { vertex =>
      hPolyhedron.inequalities.indices.filter { k =>
        val ineq = hPolyhedron.inequalities(k)
        ineq.lhs.dot(vertex) === ineq.rhs
      }.toSet
    }
    apply(hPolyhedron, zeroSets)
  }
}