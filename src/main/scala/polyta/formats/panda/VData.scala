package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

case class VData[M, V](
  dim: Option[Int] = None,
  names: Option[Seq[String]] = None,
  polyhedron: Option[VPolyhedron[M, V, Rational]] = None,
  maps: Seq[Seq[String]] = Seq.empty
)
