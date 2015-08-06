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

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

// case classes are not specialized yet, due to a bug
case class LPObjective[V](name: String, direction: Direction, f: V)
case class LPConstraint[V](name: String, constraint: LinearConstraint[V, Rational])
case class LPData[V](
  val problemName: String,
  val variableNames: Seq[String],
  val objective: LPObjective[V],
  val constraints: Seq[LPConstraint[V]],
  val bounds: Box[V, Rational],
  val integerVariables: Set[Int]
)

object LPData {
  implicit def FormatWrite[V](implicit pack: PackField.ForV[V, Rational]): FormatWrite[LPData[V]] = new LPDataWrite[V]
}
