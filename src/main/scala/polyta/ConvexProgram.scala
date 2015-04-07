package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._

trait ConvexProgram[M, V, @sp(Double) A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit def A: Field[A] = M.scalar
  def nX: Int
  def direction: Direction
  def objective: V
  def feasibleSet: ConvexSet[M, V, A]
}
