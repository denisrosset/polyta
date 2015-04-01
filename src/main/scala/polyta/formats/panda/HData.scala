package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra.VecInField

case class HData(
  dim: Option[Int] = None,
  names: Option[Seq[String]] = None,
  constraints: Seq[Constraint[Rational]] = Seq.empty,
  maps: Seq[Seq[String]] = Seq.empty
)

object HData {
  implicit def FormatRead[V](implicit V: VecInField[V, Rational]): FormatRead[HData] = new HDataRead[V]
}
