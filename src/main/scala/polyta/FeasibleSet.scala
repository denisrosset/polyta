package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._
import qalg.algos._

/** Generic feasible set base class. */
trait FeasibleSet[V, @sp(Double) A] extends Any with WithVariables {
  implicit def alg: AlgVF[V, A]
  implicit def A: Field[A] = alg.V.A
}
