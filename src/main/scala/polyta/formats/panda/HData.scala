package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

import net.alasc.math.Perm

case class HData[M, V](
  polyhedron: HPolyhedronM[M, V, Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[M, V, Rational]] = Seq.empty)

object HData {
  implicit def FormatRead[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatRead[HData[M, V]] = new HDataRead[M, V]
  implicit def FormatWrite[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatWrite[HData[M, V]] = new HDataWrite[M, V]
}
