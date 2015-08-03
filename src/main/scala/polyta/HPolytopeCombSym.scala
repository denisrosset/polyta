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

final class HPolytopeCombSym[V, @sp(Double, Long) A](val nX: Int, val inequalities: Seq[LinearInequality[V, A]], val equalities: Seq[LinearEquality[V, A]], val symGroup: Grp[Perm])(implicit val pack: PackField.ForV[V, A]) extends HPolytope[V, A] {
  type G = Perm
  def facets = inequalities.indices.map(new Facet(_))
  def symSubgroup(f: Facet): Grp[Perm] = symGroup.stabilizer(f.index)._1
  final class Facet(val index: Int) extends FacetBase[V, A] {
    def inequality: LinearInequality[V, A] = inequalities(index)
    def representatives: Iterable[Facet] = {
      import net.alasc.math.OrbitInstances._
      val orbit = Set(index) <|+| symGroup.generators
      orbit.map( new Facet(_) )
    }
  }
  object action extends Action[Facet, Perm] {
    def actr(f: Facet, p: Perm): Facet = new Facet(f.index <|+| p)
    def actl(p: Perm, f: Facet): Facet = new Facet(p |+|> f.index)
  }
}
