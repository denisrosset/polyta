package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.util._

import scalin.immutable.{Mat, Vec}

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.prep.PGrp.default._
import net.alasc.perms._
import net.alasc.util._

/** Polytope with combinatorial symmetry. The facets are described using `mA` and `vb`, such that
  * `mA * x <= vb`, and all facet representatives are present in `mA`, `vb`. The symmetry group
  * is composed of permutations that permute the facets, i.e. the rows of `mA`, `vb`.
  * 
  * Equalities are described using `mAeq`, `vbeq`, such that `mAeq * x = vbeq`; we do not consider
  * the action of symmetry group on equality constraints.
  */
final class HPolytopeCombSym[A](
  val mA: Mat[A],
  val vb: Vec[A],
  val mAeq: Mat[A],
  val vbeq: Vec[A],
  val symGroup: Grp[Perm]
)(
  implicit val A: LinAlg[A]
) extends HPolytope[A] {

  import A.IVec
  import ComparisonOp._

  type G = Perm

  def dim: Int = mA.nCols

  def nTotalFacets = mA.nRows

  val facetOrbitRepresentatives: Seq[Int] = {
    val orbits: Set[collection.immutable.BitSet] = Orbits.orbits(nTotalFacets, symGroup.generators, Perm.permutation)
    orbits.map(_.head).toSeq.sorted
  }

  final class Facet(val index: Int) extends HPolytope.Facet[A] {
    type G = Perm
    type F = Facet
    def inequality: LinearInequality[A] = LinearInequality(mA(index, ::), LE, vb(index))
    def representatives: Iterable[Facet] = {
      val orbit = Orbits.orbit(index, symGroup.generators, Perm.permutation)
      orbit.map( new Facet(_) )
    }
    def symSubgroup: Grp[G] = symGroup.in(representation).stabilizer(index)
  }

  val representation = Perm.permutationRepBuilder.forSize(nTotalFacets)

  def equalities = (0 until mAeq.nRows).view.map(i => LinearEquality(mAeq(i, ::), vbeq(i)))

  def facets = facetOrbitRepresentatives.view.map(new Facet(_))

  override def allFacets = (0 until mA.nRows).view.map(new Facet(_))

  object action extends Action[Facet, G] {
    def actr(f: Facet, g: Perm): Facet = new Facet(g.image(f.index))
    def actl(g: Perm, f: Facet): Facet = new Facet(g.invImage(f.index))
  }

  def facetIndexSet(vertex: VPolytope.Vertex[A]): Set[Int] = {
    import A.{orderA, IVec}
    val res = mA * vertex.point
    (0 until res.length).toSet.filter(i => res(i) === vb(i))
  }

}

object HPolytopeCombSym {

  def empty[A](dim: Int)(implicit A: LinAlg[A]): HPolytopeCombSym[A] =
    new HPolytopeCombSym(A.IMat.zeros(0, dim), A.IVec.zeros(0), A.IMat.zeros(0, dim), A.IVec.zeros(0), Grp.trivial[Perm])

  def apply[A](mA: Mat[A], vb: Vec[A], mAeq: Mat[A], vbeq: Vec[A], symGroup: Grp[Perm])(implicit A: LinAlg[A]): HPolytopeCombSym[A] = {
    new HPolytopeCombSym(mA, vb, mAeq, vbeq, symGroup)
  }

}
