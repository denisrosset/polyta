package com.faacets
package polyta
package solvers

trait SymmetryFinder[-Poly, +SymPoly] extends Any {
  def symmetric(p: Poly): SymPoly
}
