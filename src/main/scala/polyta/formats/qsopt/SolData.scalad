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

case class SolData[A](
  status: SolverStatus,
  optimalValue: Option[A] = None,
  variables: Map[String, A] = Map.empty[String, A],
  reducedCost: Map[String, A] = Map.empty[String, A],
  pi: Map[String, A] = Map.empty[String, A],
  slack: Map[String, A] = Map.empty[String, A])

object SolData {
  object rational {
    implicit val Read = new SolDataRead[Rational] {
      object Parser extends SolDataParser with RationalParser {
        def value = rational
      }
    }
  }
}
