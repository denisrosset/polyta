package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import qalg.algebra._

/** Generic convex set base class. */
trait ConvexSet[V, @sp(Double) A] extends Any with FeasibleSet[V, A]
