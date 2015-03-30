package com.faacets
package polyta
package formats
package qsopt

sealed trait Bound[A] {
  def variable: String
}

case class FreeBound[A](variable: String) extends Bound[A]

case class ExplicitBound[A](variable: String,
  lowerBound: Option[Extended[A]], upperBound: Option[Extended[A]]) extends Bound[A]
