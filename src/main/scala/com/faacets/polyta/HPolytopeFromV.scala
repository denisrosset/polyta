package com.faacets.polyta

import scalin.immutable.Vec
import net.alasc.perms.default._
import spire.syntax.action._
import net.alasc.std.set._

/** Does not support unbounded polytopes.
  *
  * @param vertexIndexSets Indices of the vertices that support representatives of the facets of the polytope
  *                        in the H representation. For each family of facets, a single representative is
  *                        present.
  */
final class HPolytopeFromV[A](val vPolytope: VPolytope.Aux[A, Symmetry.Combinatorial], val vertexIndexSets: Seq[Set[Int]])
                             (implicit val A: LinAlg[A]) {

  val dim = vPolytope.dim

  def nFamilies: Int = vertexIndexSets.size

  def familyRepresentative(k: Int): LinearInequality[A] = {
    val indices = vertexIndexSets(k)
    val satisfying = (0 until vPolytope.mV.nRows).find(!indices.contains(_)).get
    vPolytope.facetOnVertices(indices, satisfying)
  }

  def familyRepresentatives(k: Int): Iterable[LinearInequality[A]] = {
    val indices = vertexIndexSets(k)
    val satisfying = (0 until vPolytope.mV.nRows).find(!indices.contains(_)).get
    val subGroup = vPolytope.symmetry.group.setwiseStabilizer(indices)
    new Iterable[LinearInequality[A]] {
      override def size = (vPolytope.symmetry.group.order / subGroup.order).toInt
      def iterator = {
        val cosets = vPolytope.symmetry.group.rightCosetsBy(subGroup)
        cosets.iterator.map { coset => vPolytope.facetOnVertices(indices <|+| coset.representative,
          satisfying <|+| coset.representative)
        }
      }
    }
  }

}
