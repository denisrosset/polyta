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

/** Polytope with optional combinatorial symmetry. The facets are described using `mA` and `vb`, such that
  * `mA * x <= vb`, and all facet representatives are present in `mA`, `vb`. The symmetry group
  * is composed of permutations that permute the facets, i.e. the rows of `mA`, `vb`.
  * 
  * Equalities are described using `mAeq`, `vbeq`, such that `mAeq * x = vbeq`.
  */
final class HPolytopeM[A](
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

  def equalities: Seq[LinearEquality[A]] = (0 until mAeq.nRows).view.map(i => LinearEquality(mAeq(i, ::), vbeq(i)))

  def facets: Seq[Facet] = facetOrbitRepresentatives.view.map(new Facet(_))

  override def allFacets: Seq[Facet] = (0 until mA.nRows).view.map(new Facet(_))

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

object HPolytopeM {

  def empty[A](dim: Int)(implicit A: LinAlg[A]): HPolytopeM[A] =
    new HPolytopeM(A.IMat.zeros(0, dim), A.IVec.zeros(0), A.IMat.zeros(0, dim), A.IVec.zeros(0), Grp.trivial[Perm])

  def apply[A](mA: Mat[A], vb: Vec[A], mAeq: Mat[A], vbeq: Vec[A])(implicit A: LinAlg[A]): HPolytopeM[A] =
    apply(mA, vb, mAeq, vbeq, Grp.trivial[Perm])

  def apply[A](mA: Mat[A], vb: Vec[A], mAeq: Mat[A], vbeq: Vec[A], symGroup: Grp[Perm])(implicit A: LinAlg[A]): HPolytopeM[A] =    new HPolytopeM(mA, vb, mAeq, vbeq, symGroup)

  def apply[A](dim: Int, facets: Seq[LinearInequality[A]], equalities: Seq[LinearEquality[A]])(implicit A: LinAlg[A]): HPolytopeM[A] =
    apply(dim, facets, equalities, Grp.trivial[Perm])

  def apply[A](dim: Int, facets: Seq[LinearInequality[A]], equalities: Seq[LinearEquality[A]], symGroup: Grp[Perm])(implicit A: LinAlg[A]): HPolytopeM[A] = {
    import A.{fieldA, IMat, IVec}
    import ComparisonOp._

    val mA = IMat.tabulate(facets.size, dim) { (r, c) =>
      if (facets(r).op == LE) facets(r).lhs(c) else -facets(r).lhs(c)
    }
    val vb = IVec.tabulate(facets.size)( i => if (facets(i).op == LE) facets(i).rhs else -facets(i).rhs )
    val mAeq = IMat.tabulate(equalities.size, dim)( (r, c) => equalities(r).lhs(c) )
    val vbeq = IVec.tabulate(equalities.size)( i => equalities(i).rhs )
    apply(mA, vb, mAeq, vbeq, symGroup)
  }

  object WithoutSym {

    def intersection[A](lhs: HPolytopeM[A], rhs: HPolytopeM[A])(implicit A: LinAlg[A]): HPolytopeM[A] = {
      import scalin.syntax.all._
      import A.{IVec, IMat}
      val mA = colMat[A](lhs.mA, rhs.mA).flatten
      val vb = vec[A](lhs.vb, rhs.vb).flatten
      val mAeq = colMat[A](lhs.mAeq, rhs.mAeq).flatten
      val vbeq = vec[A](lhs.vbeq, rhs.vbeq).flatten
      HPolytopeM(mA, vb, mAeq, vbeq)
    }

  }

}
