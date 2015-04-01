package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

case class HData(
  dim: Option[Int] = None,
  names: Option[Seq[String]] = None,
  constraints: Seq[Constraint[Rational]] = Seq.empty,
  maps: Seq[Seq[String]] = Seq.empty
)
