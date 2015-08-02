package com.faacets
package polyta
package solvers

trait Symmetric[-P, +S] extends Any {
  def symmetric(polytope: P): S
}
