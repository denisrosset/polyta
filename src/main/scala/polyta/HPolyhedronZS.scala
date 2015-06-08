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
  implicit def alg: AlgMVF[M, V, A]
  implicit def orderA: Order[A]

  def vPolyhedron: VPolyhedron[V, A]
  def inequalityZeroSets: Seq[Set[Int]]

  def inequalities: Seq[LinearInequality[V, A]] = new IndexedSeq[LinearInequality[V, A]] {
    def length = inequalityZeroSets.length
    def apply(k: Int): LinearInequality[V, A] = HZeroSet[M, V, A](vPolyhedron, inequalityZeroSets(k)).inequality
  }

  def nX: Int = vPolyhedron.nX
}

object HPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](vPolyhedron0: VPolyhedron[V, A], inequalityZeroSets0: Seq[Set[Int]], equalities0: Seq[LinearEquality[V, A]])(implicit alg0: AlgMVF[M, V, A], orderA0: Order[A]): HPolyhedronZS[M, V, A] = new HPolyhedronZS[M, V, A] {
    def alg = alg0
    def orderA = orderA0
    def vPolyhedron = vPolyhedron0
    def inequalityZeroSets = inequalityZeroSets0
    def equalities = equalities0
  }

  def apply[M, V, @sp(Double, Long) A: Order](vPolyhedron: VPolyhedron[V, A], inequalityZeroSets: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgMVF[M, V, A]): HPolyhedronZS[M, V, A] = build(vPolyhedron, inequalityZeroSets, equalities)

  def fromDualDescription[M, V, @sp(Double, Long) A: Order](hPolyhedron: HPolyhedron[V, A], vPolyhedron: VPolyhedron[V, A])(implicit alg: AlgMVF[M, V, A]): HPolyhedronZS[M, V, A] = {
    val zeroSets = vPolyhedron.vertices.map { vertex =>
      hPolyhedron.inequalities.indices.filter { k =>
        val ineq = hPolyhedron.inequalities(k)
        ineq.lhs.dot(vertex) === ineq.rhs
      }.toSet
    }
    apply(vPolyhedron, zeroSets, hPolyhedron.equalities)
  }
}
