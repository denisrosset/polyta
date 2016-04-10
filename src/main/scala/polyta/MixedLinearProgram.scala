package com.faacets
package polyta

import scala.collection.BitSet


trait MixedLinearProgram[A] extends ConvexProgram[A] {

  override def toString = s"LinearProgram($direction, $objective, $feasibleSet, $bounds)"

  def feasibleSet: HPolytope[A]

  def bounds: Box[A]

  def integerVariables: BitSet

  def dim = feasibleSet.dim

  require(dim == bounds.dim)

}
