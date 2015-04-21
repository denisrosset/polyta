package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra.MatVecInField

import net.alasc.math.Perm

case class HData[M, V](
  polyhedron: HPolyhedronM[M, V, Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[Perm] = Seq.empty)

object HData {
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[HData[M, V]] = new HDataRead[M, V]
  implicit def FormatWrite[M, V](implicit M: MatVecInField[M, V, Rational]): FormatWrite[HData[M, V]] = new HDataWrite[M, V]
}
