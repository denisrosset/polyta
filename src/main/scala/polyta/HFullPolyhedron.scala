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

/** Polyhedron, i.e. possibly unbounded intersection of half-spaces, given by the
  * equations:
  * 
  * - mA * x <= vb
  * - mAeq * x = vbeq
  */
trait HFullPolyhedron[M, V, @sp(Double) A] extends HPolyhedron[M, V, A] {
  override def toString =
    ((0 until nIneqs).map( r => mA(r,::).toString + " <= " + vb(r).toString ) ++
      (0 until nEqs).map( r => mAeq(r,::).toString + " == " + vbeq(r).toString )).mkString("\n")
  def mA: M
  def vb: V
  def mAeq: M = M.zeros(0, nX)
  def vbeq: V = V.zeros(0)
  override def nEqs: Int = 0
}

object HFullPolyhedron {
  def apply[M, V, @sp(Double) A](mA0: M, vb0: V)(implicit M0: MatVecInField[M, V, A]): HFullPolyhedron[M, V, A] =
    new HFullPolyhedron[M, V, A] {
      def M = M0
      def mA = mA0
      def vb = vb0
    }
}
