package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._

/** Generic feasible set base class. */
trait FeasibleSet[V, @sp(Double) A] extends Any with WithVariables {
  implicit def V: VecInField[V, A]
  implicit def A: Field[A] = V.scalar
}
