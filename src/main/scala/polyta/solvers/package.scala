package com.faacets.polyta

import scala.{specialized => sp}

package object solvers {
  implicit class DualOps[P, D](val polytope: P)(implicit ev: Dual[P, D]) {
    def dual: D = ev.dual(polytope)
  }
  implicit class ReducedDualOps[P, R](val polytope: P)(implicit ev: ReducedDual[P, R]) {
    def reducedDual: R = ev.reducedDual(polytope)
  }
  implicit class SymmetryFinderOps[P, S](val polytope: P)(implicit ev: SymmetryFinder[P, S]) {
    def symmetric: S = ev.symmetric(polytope)
  }
}
