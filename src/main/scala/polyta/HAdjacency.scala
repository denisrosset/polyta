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

trait HAdjacency[V, @sp(Double) A] {
  def hPolyhedron: HPolyhedron[V, A]
  def zeroSet: Set[Int]
  def vertex: V
}

final class HAdjacencyImpl[M, V, @sp(Double) A: Order](val hPolyhedron: HPolyhedron[V, A], val zeroSet: Set[Int])(implicit alg: AlgMVF[M, V, A]) extends HAdjacency[V, A] {
  implicit def A: Field[A] = alg.V.A

  override def toString: String = zeroSet.toSeq.sorted.mkString("VZeroSet(", ", ", ")")

  def vertex: V = {
    val ineqSatisfied: Seq[(V, A)] = zeroSet.toSeq.map {
      i => (hPolyhedron.facets(i).lhs, hPolyhedron.facets(i).rhs)
    }

    val eqSatisfied: Seq[(V, A)] = hPolyhedron.equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA: M = MatBuilder[M, A].fromRows(hPolyhedron.nX, satisfied.map(_._1): _*)
    val newb: V = VecBuilder[V, A].build(satisfied.map(_._2): _*)
    newA.lu.solveV(newb)
  }
}

object HAdjacency {
  def apply[V, @sp(Double) A: Order, M](hPolyhedron: HPolyhedron[V, A], zeroSet: Set[Int])(implicit alg: AlgMVF[M, V, A]): HAdjacency[V, A] = new HAdjacencyImpl[M, V, A](hPolyhedron, zeroSet)
}
