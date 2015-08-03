package com.faacets
package polyta

import scala.{specialized => sp}

/** Generic convex set base class. */
trait ConvexSet[V, @sp(Double) A] extends Any with FeasibleSet[V, A]
