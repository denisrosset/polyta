package com.faacets
package polyta
package formats
package qsopt

case class NamedObjective[A](
  direction: Direction,
  f: Map[String, A]) {
  def variables: Set[String] = f.keys.toSet
}
