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

/** Cone defined by Aplus * x >= 0. */
trait HCone[M, V, @sp(Double) A] extends HPolyhedron[M, V, A] {
  def mAplus: M
  def mA: M = -mAplus
  def vb: V = V.zeros(nIneqs)
  def mAeq: M = MV.zeros(0, nX)
  def vbeq: V = V.zeros(0)
}

object HCone {
  def apply[M, V, @sp(Double) A](mAplus0: M)(implicit MV0: MatVecInField[M, V, A]): HCone[M, V, A] =
    new HCone[M, V, A] {
      def MV = MV0
      def mAplus = mAplus0
    }
}
