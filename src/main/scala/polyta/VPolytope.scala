package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.all._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

import solvers._

/** Bounded polyhedron described by extremal vertices. */
trait VPolytope[V, @sp(Double) A] extends VPolyhedron[V, A] { lhs =>

  override def toString =
    "\nVertices:\n" + vertices.mkString("\n")
  def vertices: Seq[V]
  def rays: Seq[V] = Seq.empty[V]

  def nX: Int
  // TODO: type class syntax
  //  def symmetric(implicit S: SymmetryFinder[VPolyhedron[V, A], SymVPolyhedron[V, A]]): SymVPolyhedron[V, A] = S.symmetric(lhs)
  // def toH[HP](implicit C: VConverter[VPolytope[V, A], HP]): HP = C.toH(lhs)
}

final class VPolytopeImpl[V, @sp(Double) A](val vertices: Seq[V])(implicit val alg: AlgVF[V, A]) extends VPolytope[V, A] {
  require(vertices.nonEmpty)
  def nX = vertices.head.length
}

object VPolytope {
  def apply[V, A: Order](vertices: Seq[V])(implicit alg: AlgVF[V, A]): VPolytope[V, A] = new VPolytopeImpl(vertices)
}
