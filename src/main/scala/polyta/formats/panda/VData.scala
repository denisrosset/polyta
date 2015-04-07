package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra.MatVecInField

import net.alasc.math.Perm

case class VData[M, V](
  polyhedron: VPolyhedron[M, V, Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[Perm] = Seq.empty)

object VData {
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[VData[M, V]] = new VDataRead[M, V]
}
