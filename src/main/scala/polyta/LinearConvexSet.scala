package com.faacets
package polyta

import scala.{specialized => sp}

trait LinearConvexSet[M, V, @sp(Double) A] extends Any with ConvexSet[M, V, A]
