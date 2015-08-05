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
import qalg.syntax.indup.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._

/** Polytope with combinatorial symmetry. The facets are described using `mA` and `vb`, such that
  * `mA * x <= vb`, and all facet representatives are present in `mA`, `vb`. The symmetry group
  * is composed of permutations that permute the facets, i.e. the rows of `mA`, `vb`.
  * 
  * Equalities are described using `mAeq`, `vbeq`, such that `mAeq * x = vbeq`; we do not consider
  * the action of symmetry group on equality constraints.
  */
final class HPolytopeCombSym[M, V, @sp(Double, Long) A, G0](
  val mA: M,
  val vb: V,
  val mAeq: M,
  val vbeq: V,
  val symGroup: Grp[G0],
  val facetIndexAction: FaithfulPermutationAction[G0],
  val facetOrbitRepresentatives: Seq[Int])(implicit val pack: PackField.ForMV[M, V, A], val orderA: Order[A]) extends HPolytope[V, A] {

  type G = G0

  def nX: Int = mA.nCols

  def totalFacets = mA.nRows

  object representation extends Representation[G] {
    def size = totalFacets
    def action = facetIndexAction
    def represents(g: G) = true
    def representations = Opt.empty
  }

  def facets = facetOrbitRepresentatives.view.map(new Facet(_))

  override def allFacets = (0 until mA.nRows).view.map(new Facet(_))

  def equalities = (0 until mAeq.nRows).view.map(i => LinearEquality(mAeq(i, ::), vbeq(i)))

  final class Facet(val index: Int) extends FacetBase[V, A, G] {
    type F = Facet
    def inequality: LinearInequality[V, A] = LinearInequality(mA(index, ::), LE, vb(index))
    def representatives: Iterable[Facet] = {
      val orbit = Orbits.orbit(index, symGroup.generators, facetIndexAction)
      orbit.map( new Facet(_) )
    }
    def symSubgroup: Grp[G] = symGroup.stabilizer(index, representation)._1
  }

  def facetIndexSet(vertex: VertexBase[V, _]): Set[Int] = {
    import pack.A
    val res = (mA ::* vertex.point) - vb
    (0 until res.length).toSet.filter(i => res(i).isZero)
  }

  object action extends Action[Facet, G] {
    def actr(f: Facet, g: G): Facet = new Facet(facetIndexAction.actr(f.index, g))
    def actl(g: G, f: Facet): Facet = new Facet(facetIndexAction.actl(g, f.index))
  }
}

object HPolytopeCombSym {
  def apply[M, V, A: Order, G](mA: M, vb: V, mAeq: M, vbeq: V, symGroup: Grp[G], facetAction: FaithfulPermutationAction[G])(implicit pack: PackField.ForMV[M, V, A]): HPolytopeCombSym[M, V, A, G] = {
    val totalFacets = mA.nRows
    val orbits: Set[collection.immutable.BitSet] = Orbits.orbits(totalFacets, symGroup.generators, facetAction)
    val orbitRepresentatives = orbits.map(_.head).toSeq.sorted
    new HPolytopeCombSym(mA, vb, mAeq, vbeq, symGroup, facetAction, orbitRepresentatives)
  }
  def apply[M, V, A: Order, P: PermutationRepresentations](mA: M, vb: V, mAeq: M, vbeq: V, symGroup: Grp[P])(implicit pack: PackField.ForMV[M, V, A]): HPolytopeCombSym[M, V, A, P] = {
    val action = PermutationRepresentations[P].forSize(mA.nRows).action
    HPolytopeCombSym(mA, vb, mAeq, vbeq, symGroup, action)
  }
}
