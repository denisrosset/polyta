package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

trait HConverter[-HPoly, +VPoly] {
  def toV(polyhedron: HPoly): VPoly
}
