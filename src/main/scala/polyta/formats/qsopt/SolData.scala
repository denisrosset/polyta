package com.faacets
package polyta
package formats
package qsopt

import scala.{specialized => sp}

import scala.util.parsing.combinator._

import scala.collection.BitSet

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import solvers._

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
