package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait SymHPolyhedronZS[M, V, @sp(Double) A] extends SymHPolyhedron[V, A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def MM: MatMutable[M, A]
  implicit def orderA: Order[A]

  def vPolyhedron: SymVPolyhedron[V, A]
  def inequalityFamilyZeroSets: Seq[Set[Int]]

  case class Family(representative: Set[Int]) extends IndexedSeq[LinearInequality[V, A]] {
    lazy val symmetrySubgroup = symmetryGroup.setwiseStabilizer(representative.toSeq: _*)
    lazy val cosets = (symmetrySubgroup \ symmetryGroup).iterator.toSeq
    def length = cosets.length
    def apply(k: Int): LinearInequality[V, A] = {
      val g = cosets(k).g
      val zeroSeq = representative.toSeq.map(_ <|+| g)
      HZeroSet[M, V, A](vPolyhedron, zeroSeq.toSet).inequality
    }
  }

  def nX: Int = vPolyhedron.nX
}
/*
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
 */