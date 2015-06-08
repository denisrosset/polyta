package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

import net.alasc.math.Perm

case class VData[M, V](
  polyhedron: VPolyhedronM[M, V, Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[M, V, Rational]] = Seq.empty)

object VData {
  implicit def FormatRead[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatRead[VData[M, V]] = new VDataRead[M, V]
  implicit def FormatWrite[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatWrite[VData[M, V]] = new VDataWrite[M, V]
}
