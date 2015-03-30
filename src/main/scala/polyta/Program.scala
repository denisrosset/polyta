package com.faacets
package polyta

import spire.algebra._
import qalg.algebra._

import scala.{specialized => sp}

trait Program[M, V, @sp(Double) A] extends Any with WithVariables {
  implicit def MV: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = MV.V
  implicit def A: Field[A] = MV.scalar
  def direction: Direction
  def objective: V
  def feasibleSet: FeasibleSet[M, V, A]
}
