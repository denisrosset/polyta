package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

trait VConverter[-VPoly, +HPoly] {
  def toH(polyhedron: VPoly): HPoly
}
