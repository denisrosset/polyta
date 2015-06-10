package com.faacets
package polyta
package solvers

trait ReducedDual[P, R] extends Any {
  def reducedDual(polytope: P): R
}
