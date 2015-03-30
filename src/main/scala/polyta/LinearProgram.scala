package com.faacets
package polyta

import scala.{specialized => sp}

import scala.collection.BitSet

import spire.algebra.Eq
import spire.math.Rational
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

trait LinearProgram[M, V, @sp(Double) A] extends MixedLinearProgram[M, V, A] {
  override def toString = s"LinearProgram($direction, $objective, $feasibleSet, $bounds)"
  def feasibleSet: HPolyhedron[M, V, A]
  def bounds: Box[M, V, A]
  def integerVariables: BitSet = BitSet.empty
  require(nX == bounds.nX)
}

object LinearProgram {
  def apply[M, V, @sp(Double) A](direction0: Direction, objective0: V, feasibleSet0: HPolyhedron[M, V, A], bounds0: Box[M, V, A])(implicit MV0: MatVecInField[M, V, A]): LinearProgram[M, V, A] =
    new LinearProgram[M, V, A] {
      def MV = MV0
      def direction = direction0
      def objective = objective0
      def feasibleSet = feasibleSet0
      def bounds = bounds0
    }
}
