package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

case class VData(
  dim: Option[Int] = None,
  names: Option[Seq[String]] = None,
  polyhedron: Option[VPolyhedron[M, V, Rational]],
  maps: Seq[Seq[String]] = Seq.empty
)
