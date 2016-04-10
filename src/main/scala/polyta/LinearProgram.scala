package com.faacets
package polyta

import scala.collection.BitSet

import scalin.Vec

trait LinearProgram[A] extends MixedLinearProgram[A] {

  override def toString = s"LinearProgram($direction, $objective, $feasibleSet, $bounds)"

  def feasibleSet: HPolytope[A]

  def bounds: Box[A]

  def integerVariables: BitSet = BitSet.empty

  require(dim == bounds.dim)

}

object LinearProgram {

  def apply[A](direction0: Direction, objective0: Vec[A], feasibleSet0: HPolytope[A], bounds0: Box[A])(implicit A0: LinAlg[A]): LinearProgram[A] =
    new LinearProgram[A] {
      def A = A0
      def direction = direction0
      def objective = objective0
      def feasibleSet = feasibleSet0
      def bounds = bounds0
    }

}
