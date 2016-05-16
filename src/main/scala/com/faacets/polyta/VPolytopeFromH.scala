package com.faacets.polyta

import spire.algebra.Action

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._
import spire.syntax.action._
import net.alasc.std.set._
import scalin.immutable.Vec

/** Polytope in the V representation, obtained from the conversion of a polytope in the
  * H representation.
  * Does not (yet) support unbounded polytopes (i.e. with rays).
  *
  * @param hPolytope      Polytope in the H representation with a possible combinatorial symmetry.
  * @param facetIndexSets Indices of the facets that support representatives of the vertices of the polytope
  *                       in the V representation. For each family of vertices, a single representative is
  *                       present.
  */
final class VPolytopeFromH[A](val hPolytope: HPolytope.Aux[A, Symmetry.Combinatorial], val facetIndexSets: Seq[Set[Int]])
                       (implicit val A: LinAlg[A]) {

  val dim = hPolytope.dim

  def nFamilies: Int = facetIndexSets.size

  def familyRepresentative(k: Int): Vec[A] =
    hPolytope.vertexOn(facetIndexSets(k))

  def familyRepresentatives(k: Int): Iterable[Vec[A]] = {
    val indices = facetIndexSets(k)
    val subGroup = hPolytope.symmetry.group.setwiseStabilizer(indices)
    new Iterable[Vec[A]] {
      override def size = (hPolytope.symmetry.group.order / subGroup.order).toInt
      def iterator = {
        val cosets = hPolytope.symmetry.group.rightCosetsBy(subGroup)
        cosets.iterator.map { coset => hPolytope.vertexOn(indices <|+| coset.representative) }
      }
    }
  }

}
