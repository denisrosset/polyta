package com.faacets
package polyta
package formats
package sympol

import spire.algebra._
import spire.math.SafeLong
import spire.syntax.group._

import net.alasc.perms.Perm
import net.alasc.syntax.all._

/** Information about the symmetries of a polytope
  * 
  * @param upToSymmetryWRTO Up to symmetry, with respect to the original inequalities/vertices.
  * @param generators       Generators for permutations with respect to the original 
  *                         ordering of vertices/rays or (in)equalities, either in the
  *                         current polyhedron, or the dual description.
  */
case class SymmetryInfo(
  upToSymmetryWRTO: Boolean,
  order: Option[SafeLong],
  generators: Seq[Perm],
  base: Seq[Int]) {

  def decodeGenerators(rayEqualityRows: Set[Int]): Seq[(Perm, Perm)] =
    if (rayEqualityRows.isEmpty) generators.map(g => (g, Group[Perm].id)) else {
      val nEq = rayEqualityRows.size
      val otherRows = (0 to rayEqualityRows.max).toSet -- rayEqualityRows
      val orderedToUnorderedImages = rayEqualityRows.toSeq.sorted ++ otherRows.toSeq.sorted
      val orderedToUnordered = Perm.fromImages(orderedToUnorderedImages)
      val unorderedToOrdered = orderedToUnordered.inverse
      generators.map { g =>
        val orderedG = orderedToUnordered |+| g |+| unorderedToOrdered
        val images = orderedG.images(orderedG.largestMovedPoint.getOrElseFast(nEq - 1) + 2)
        val (imagesER, imagesVI) = images.splitAt(nEq)
        val permER = Perm.fromImages(imagesER)
        val permVI = Perm.fromImages(imagesVI.map(_ - nEq))
        (permVI, permER)
      }
    }
  
}

object SymmetryInfo {

  implicit val formatRead: FormatRead[SymmetryInfo] = new SymmetryInfoRead

}
