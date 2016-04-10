package com.faacets
package polyta
package formats
package qsopt

import spire.math.Rational

case class SolData(
  status: SolverStatus[String],
  optimalValue: Option[Rational] = None,
  variables: Map[String, Rational] = Map.empty[String, Rational],
  reducedCost: Map[String, Rational] = Map.empty[String, Rational],
  pi: Map[String, Rational] = Map.empty[String, Rational],
  slack: Map[String, Rational] = Map.empty[String, Rational])

object SolData {

  implicit object FormatRead extends SolDataRead

}
