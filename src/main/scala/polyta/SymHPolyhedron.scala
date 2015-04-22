package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait SymHPolyhedron[V, @sp(Double) A] extends HPolyhedron[V, A] {
  def symmetryGroup: Grp[Perm]
}

object SymHPolyhedron {
  @inline protected def build[V, A](inequalities0: Seq[LinearInequality[V, A]], equalities0: Seq[LinearEquality[V, A]], symmetryGroup0: Grp[Perm])(implicit V0: VecInField[V, A]): SymHPolyhedron[V, A] =
    new SymHPolyhedron[V, A] {
      def V = V0
      def inequalities = inequalities0
      def equalities = equalities0
      def symmetryGroup = symmetryGroup0
      def nX = inequalities.head.lhs.length
    }
  def apply[V, A](inequalities: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]], symmetryGroup: Grp[Perm])(implicit V: VecInField[V, A]): SymHPolyhedron[V, A] =
    build(inequalities, equalities, symmetryGroup)
}
