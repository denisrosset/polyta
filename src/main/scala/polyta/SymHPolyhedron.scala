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
  def symmetryGroup: Grp[(Perm, Perm)]
  def equalityFamilies: IndexedSeq[IndexedSeq[LinearEquality[V, A]]]
  def inequalityFamilies: IndexedSeq[IndexedSeq[LinearInequality[V, A]]]
}
