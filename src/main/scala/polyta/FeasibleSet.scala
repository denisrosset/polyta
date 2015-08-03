package com.faacets
package polyta

import scala.{specialized => sp}

/** Generic feasible set base class. */
trait FeasibleSet[V, @sp(Double) A] extends Any with WithVariables
