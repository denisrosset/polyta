package com.faacets
package polyta
package solvers

trait SymmetryFinder[P, S] extends Any {
  def symmetric(polytope: P): S
}
