package com.faacets
package polyta
package solvers

trait Dual[-P, +D] {
  def dual(polyhedron: P): D
}
