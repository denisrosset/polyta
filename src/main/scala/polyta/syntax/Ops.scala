package com.faacets.polyta
package syntax

import solvers._

final class DualOps[P, D](val lhs: P)(implicit ev: Dual[P, D]) {
  def dual: D = ev.dual(lhs)
}

final class ReducedDualOps[P, R](val lhs: P)(implicit ev: ReducedDual[P, R]) {
  def reducedDual: R = ev.reducedDual(lhs)
}

final class SymmetricOps[P, S](val lhs: P)(implicit ev: Symmetric[P, S]) {
  def symmetric: S = ev.symmetric(lhs)
}
