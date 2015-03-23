package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._

trait ApproxOrder[@sp(Double) A] extends PartialOrder[A]
