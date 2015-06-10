package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

trait Dual[Poly, PolyDual] {
  def dual(polyhedron: Poly): PolyDual
}
