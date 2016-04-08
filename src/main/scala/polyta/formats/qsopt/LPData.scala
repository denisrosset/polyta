package com.faacets
package polyta
package formats
package qsopt

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import scalin.{Mat, Vec}

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
