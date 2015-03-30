package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._

/** Generic feasible set base class. */
trait FeasibleSet[M, V, @sp(Double) A] extends Any with WithVariables {
  implicit def MV: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = MV.V
  implicit def A: Field[A] = MV.scalar
}
