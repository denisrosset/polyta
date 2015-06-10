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

/** Vertices are stored as row vectors. */
trait VPolytopeM[M, V, @sp(Double, Long) A] extends VPolytope[V, A] {
  implicit def alg: AlgMVF[M, V, A]

  def vertices: IndexedSeq[V] = new IndexedSeq[V] {
    def length = mV.nCols
    def apply(c: Int): V = mV(::, c)
  }

  def mV: M

  def nX: Int = mV.nRows
}

object VPolytopeM {
  def apply[M, V, @sp(Double) A](mV: M)(implicit alg: AlgMVF[M, V, A]): VPolytopeM[M, V, A] = new VPolytopeMImpl(mV)
}

final class VPolytopeMImpl[M, V, @sp(Double) A](val mV: M)(implicit val alg: AlgMVF[M, V, A]) extends VPolytopeM[M, V ,A]
