package com.faacets
package polyta
package formats
package qsopt

case class Objective[A](
  name: Option[String],
  direction: Direction,
  f: Map[String, A]) {
  def variables: Set[String] = f.keys.toSet ++ name
}
