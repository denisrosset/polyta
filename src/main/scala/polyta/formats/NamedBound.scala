package com.faacets
package polyta
package formats
package qsopt

case class NamedBound[A](variable: String, lowerBound: Option[Extended[A]], upperBound: Option[Extended[A]])
