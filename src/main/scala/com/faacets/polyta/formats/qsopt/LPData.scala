package com.faacets
package polyta
package formats
package qsopt

import spire.math.Rational

import scalin.Vec

case class LPObjective(name: String, direction: Direction, f: Vec[Rational])

case class LPConstraint(name: String, constraint: LinearConstraint[Rational])

case class LPData(
  val problemName: String,
  val variableNames: Seq[String],
  val objective: LPObjective,
  val constraints: Seq[LPConstraint],
  val integerVariables: Set[Int]
)

object LPData {

  implicit val FormatWrite: FormatWrite[LPData] = new LPDataWrite

}
