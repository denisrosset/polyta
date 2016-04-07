package com.faacets
package polyta
package formats

case class NamedBound[A](variable: String, lowerBound: Option[Extended[A]], upperBound: Option[Extended[A]])
