package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

trait Dual[P, D] {
  def dual(polyhedron: P): D
}
