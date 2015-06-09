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

import solvers._

/** Bounded polyhedron, described using a set of inequality and equality constraints. */
trait HPolytope[V, @sp(Double, Long) A] extends HPolyhedron[V, A]

final class HPolytopeImpl[V, @sp(Double, Long) A](val facets: Seq[LinearInequality[V, A]], val equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]) extends HPolytope[V, A] {
  require(facets.nonEmpty)
  def nX = facets.head.lhs.length
}


object HPolytope {
  def apply[V, A](facets: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]): HPolytope[V, A] = new HPolytopeImpl(facets, equalities)
}
