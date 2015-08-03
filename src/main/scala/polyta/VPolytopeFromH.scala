package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._

/** Does not support unbounded polytopes. */
trait VPolytopeFromH[V, @sp(Double, Long) A] extends VPolytope[V, A] {
  def hPolytope: HPolytopeCombSym[V, A]
  def nX = hPolytope.nX
  def vertexFacetIndices: Seq[Set[Int]]
  def vertices = vertexFacetIndices.map(new Vertex(_))
  def rays = Seq.empty
  type G = Perm
  def symGroup = hPolytope.symGroup
  def symSubgroup(e: Element): Grp[G] = e match {
    case v: Vertex => symGroup.setwiseStabilizer(v.facetIndices)
    case _ => symGroup
  }
  object elementAction extends Action[Element, G] {
    def actr(e: Element, p: Perm): Element = e match {
      case v: Vertex => vertexAction.actr(v, p)
      case _ => e
    }
    def actl(p: Perm, e: Element) = e match {
      case v: Vertex => vertexAction.actl(p, v)
      case _ => e
    }
  }
  object vertexAction extends Action[Vertex, G] {
    def actr(v: Vertex, p: Perm): Vertex = new Vertex(v.facetIndices.map(_ <|+| p))
    def actl(p: Perm, v: Vertex): Vertex = new Vertex(v.facetIndices.map(p |+|> _))
  }
  object rayAction extends Action[Ray, G] {
    def actr(r: Ray, p: Perm): Ray = r
    def actl(p: Perm, r: Ray): Ray = r
  }
  trait Element extends ElementBase[V]
  type Ray = Nothing
  final class Vertex(val facetIndices: Set[Int]) extends Element with VertexBase[V] {
    def point = hPolytope.vertexOn(facetIndices)
    def representatives = new Iterable[Vertex] {
      val subgrp = hPolytope.symGroup.setwiseStabilizer(facetIndices)
      override def size = (hPolytope.symGroup.order / subgrp.order).toInt
      def iterator = {
        val cosets = subgrp \ hPolytope.symGroup
        cosets.iterator.map { coset => new Vertex(facetIndices.map(_ <|+| coset.g)) }
      }
    }
  }
}

object VPolytopeFromH {
  final class Impl[V, @sp(Double, Long) A](val hPolytope: HPolytopeCombSym[V, A], val vertexFacetIndices: Seq[Set[Int]])(implicit val pack: PackField.ForV[V, A], val orderA: Order[A]) extends VPolytopeFromH[V, A] {

  }
  def apply[V, @sp(Double, Long) A: Order](hPolytope: HPolytopeCombSym[V, A], vertexFacetIndices: Seq[Set[Int]])(implicit pack: PackField.ForV[V, A]) : VPolytopeFromH[V, A] = new Impl(hPolytope, vertexFacetIndices)
}
