package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait HPolyhedronZS[M, V, @sp(Double) A] extends HPolyhedron[V, A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def MM: MatMutable[M, A]
  implicit def orderA: Order[A]

  def vPolyhedron: VPolyhedron[V, A]
  def inequalityZeroSets: Seq[Set[Int]]

  def inequalities: Seq[LinearInequality[V, A]] = new IndexedSeq[LinearInequality[V, A]] {
    def length = inequalityZeroSets.length
    def apply(k: Int): LinearInequality[V, A] = {
      val zeroSet = inequalityZeroSets(k)
      val zeroSeq = zeroSet.toSeq
      val zeroH = vPolyhedron.vertices(zeroSeq.head)
      val zeroT = zeroSeq.tail
      val zeroVertices = zeroT.toSeq.map(vPolyhedron.vertices(_) - zeroH)
      val nonZeroIndex = (vPolyhedron.vertices.indices.toSet -- zeroSet).head
      val nonZeroVertex = vPolyhedron.vertices(nonZeroIndex) - zeroH
      val space = M.fromRows(nX, zeroVertices :+ nonZeroVertex: _*)
      val gram = gramSchmidt(space)
      val lhs = gram(zeroSet.size - 1, ::)
      val rhs = lhs.dot(zeroH)
      if (lhs.dot(nonZeroVertex) < rhs)
        LinearInequalityLE(lhs, rhs)
      else
        LinearInequalityLE(-lhs, -rhs)
    }
  }

  def nX: Int = vPolyhedron.nX
}

object HPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](vPolyhedron0: VPolyhedron[V, A], inequalityZeroSets0: Seq[Set[Int]], equalities0: Seq[LinearEquality[V, A]])(implicit M0: MatVecInField[M, V, A], MM0: MatMutable[M, A], orderA0: Order[A]): HPolyhedronZS[M, V, A] = new HPolyhedronZS[M, V, A] {
    def M = M0
    def MM = MM0
    def V = M0.V
    def orderA = orderA0
    def vPolyhedron = vPolyhedron0
    def inequalityZeroSets = inequalityZeroSets0
    def equalities = equalities0
  }

  def apply[M, V, @sp(Double, Long) A](vPolyhedron: VPolyhedron[V, A], inequalityZeroSets: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], orderA: Order[A]): HPolyhedronZS[M, V, A] = build(vPolyhedron, inequalityZeroSets, equalities)

  def fromDualDescription[M, V, @sp(Double, Long) A](hPolyhedron: HPolyhedron[V, A], vPolyhedron: VPolyhedron[V, A])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], VM: VecMutable[V, A], orderA: Order[A]): HPolyhedronZS[M, V, A] = {
    import M.V
    val zeroSets = vPolyhedron.vertices.map { vertex =>
      hPolyhedron.inequalities.indices.filter { k =>
        val ineq = hPolyhedron.inequalities(k)
        ineq.lhs.dot(vertex) === ineq.rhs
      }.toSet
    }
    apply(vPolyhedron, zeroSets, hPolyhedron.equalities)
  }
}
