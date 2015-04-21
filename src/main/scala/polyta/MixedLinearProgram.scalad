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

trait MixedLinearProgram[M, V, @sp(Double) A] extends ConvexProgram[M, V, A] {
  override def toString = s"LinearProgram($direction, $objective, $feasibleSet, $bounds)"
  def feasibleSet: HPolyhedron[M, V, A]
  def bounds: Box[M, V, A]
  def integerVariables: BitSet
  def nX = feasibleSet.nX
  require(nX == bounds.nX)
}
