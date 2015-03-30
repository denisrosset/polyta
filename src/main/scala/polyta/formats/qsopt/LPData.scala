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

case class LPData[A: Order](
  problemName: Option[String],
  objective: Objective[A],
  constraints: Seq[Constraint[A]],
  bounds: Seq[Bound[A]],
  integerVariables: Seq[String]
) {
  def variables: Set[String] =
    (Set.empty[String]
      ++ objective.variables
      ++ constraints.iterator.flatMap(_.variables)
      ++ bounds.iterator.map(_.variable)
      ++ integerVariables
    )
}

object LPData {
  object rational {
    implicit val Writer = new LPDataWrite[Rational] {
      def A = Field[Rational]
      def orderA = Order[Rational]
    }
  }
}
