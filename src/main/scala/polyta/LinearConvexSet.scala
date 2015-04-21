package com.faacets
package polyta

import scala.{specialized => sp}

trait LinearConvexSet[V, @sp(Double) A] extends Any with ConvexSet[V, A]
