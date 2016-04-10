package com.faacets
package polyta

import spire.algebra._
import spire.std.tuples._
import spire.syntax.order._
import spire.syntax.vectorSpace._

import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._

import net.alasc.finite._
import net.alasc.prep._
import net.alasc.prep.PGrp.default._
import net.alasc.perms._

import PermPerm._

/** V-representation polytope with (possible) combinatorial symmetry on its vertices.
  * 
  * @param mV                         Matrix whose rows are vertices of the polytope
  * @param mR                         Matrix whose rows are rays of the polytope
  * @param symGroup                   Symmetry group of the polytope
  * @param vertexOrbitRepresentatives Indices of the representative vertices under symmetry
  * @param rayOrbitRepresentatives    Indices of the representative rays under symmetry
  */
final class VPolytopeM[A](
  val mV: Mat[A],
  val mR: Mat[A],
  val symGroup: Grp[(Perm, Perm)]
)(
  implicit val A: LinAlg[A]
) extends VPolytope[A] {

  require(mV.nCols == mR.nCols)

  type G = (Perm, Perm)

  def dim = mV.nCols

  def nTotalVertices = mV.nRows
  def nTotalRays = mR.nRows

  def representation = PermPermPRepBuilder.PermPermPRep(nTotalVertices, nTotalRays)

  def vertices = vertexOrbitRepresentatives.view.map(new Vertex(_))

  def rays = rayOrbitRepresentatives.view.map(new Ray(_))

  override def allVertices = (0 until nTotalVertices).view.map(new Vertex(_))

  override def allRays = (0 until nTotalRays).view.map(new Ray(_))

  val vertexOrbitRepresentatives: Seq[Int] = {
    val vertexOrbits = Orbits.orbits(nTotalVertices, symGroup.generators.map(_._1), Perm.permutation)
    vertexOrbits.map(_.head).toSeq.sorted
  }

  val rayOrbitRepresentatives: Seq[Int] = {
    val rayOrbits = Orbits.orbits(nTotalRays, symGroup.generators.map(_._2), Perm.permutation)
    rayOrbits.map(_.head).toSeq.sorted
  }

  def vertexIndexSet(facet: HPolytope.Facet[A]): Set[Int] = {
    import A.{IVec, orderA}
    val ineq = facet.inequality
    val res = mV * ineq.lhs // vertices are sorted as rows
    (0 until res.length).toSet.filter(i => res(i) === ineq.rhs)
  }

  /** TODO: what happens with rays ? */
  def facetOnVertices(onVertices: Seq[Vertex], satisfying: Vertex): LinearInequality[A] = {
    import A.{fieldA, orderA}
    import A.{IMat, IVec}
    val zeroH = onVertices.head.point
    val zeroT = onVertices.tail.map(_.point)
    val nonZeroVertex = satisfying.point - zeroH
    val ortho = IMat.tabulate(zeroT.size + 1, dim) {
      (r, c) => if (r < zeroT.size) zeroT(r)(c) - zeroH(c) else nonZeroVertex(c)
    }.orthogonalized
    val lhs = ortho(ortho.nRows - 1, ::)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequality(lhs, ComparisonOp.LE,  rhs)
    else
      LinearInequality(-lhs, ComparisonOp.LE, -rhs)
  }

  sealed trait Element extends VPolytope.Element[A] {

    type G = (Perm, Perm)

  }

  final class Vertex(val index: Int) extends Element with VPolytope.Vertex[A] {
    import A.IVec
    type E = Vertex
    def point: Vec[A] = mV(index, ::)
    def representatives: Iterable[Vertex] = {
      val orbit = Orbits.orbit(index, symGroup.generators.map(_._1), Perm.permutation)
      orbit.map( new Vertex(_) )
    }
    def symSubgroup: Grp[G] = symGroup.in(representation).stabilizer(index)
  }

  final class Ray(val index: Int) extends Element with VPolytope.Ray[A] {
    import A.IVec
    type E = Ray
    def point: Vec[A] = mR(index, ::)
    def representatives: Iterable[Ray] = {
      val orbit = Orbits.orbit(index, symGroup.generators.map(_._2), Perm.permutation)
      orbit.map( new Ray(_) )
    }
    def symSubgroup: Grp[G] = symGroup.in(representation).stabilizer(index + nTotalVertices)
  }

  implicit object elementAction extends Action[Element, (Perm, Perm)] {
    def actr(e: Element, g: (Perm, Perm)): Element = e match {
      case v: Vertex => vertexAction.actr(v, g)
      case r: Ray => rayAction.actr(r, g)
    }
    def actl(g: (Perm, Perm), e: Element): Element = e match {
      case v: Vertex => vertexAction.actl(g, v)
      case r: Ray => rayAction.actl(g, r)
    }
  }

  implicit object vertexAction extends Action[Vertex, (Perm, Perm)] {
    def actr(v: Vertex, g: (Perm, Perm)): Vertex = new Vertex(g._1.image(v.index))
    def actl(g: (Perm, Perm), v: Vertex): Vertex = new Vertex(g._1.invImage(v.index))
  }

  implicit object rayAction extends Action[Ray, (Perm, Perm)] {
    def actr(r: Ray, g: (Perm, Perm)): Ray = new Ray(g._2.image(r.index))
    def actl(g: (Perm, Perm), r: Ray): Ray = new Ray(g._2.invImage(r.index))
  }

}

object VPolytopeM {

  def apply[A](dim: Int, vertexPoints: Seq[Vec[A]], rayPoints: Seq[Vec[A]])(implicit A: LinAlg[A]): VPolytopeM[A] = {
    import A.IMat
    val vertexM = IMat.tabulate(vertexPoints.size, dim)( (r, c) => vertexPoints(r)(c) )
    val rayM = IMat.tabulate(rayPoints.size, dim)( (r, c) => rayPoints(r)(c) )
    apply(vertexM, rayM)
  }

  def apply[A](allVertexPoints: Mat[A], allRayPoints: Mat[A], symGroup: Grp[(Perm, Perm)] = Grp.trivial[(Perm, Perm)])(implicit A: LinAlg[A]): VPolytopeM[A] =
    new VPolytopeM(allVertexPoints, allRayPoints, symGroup)

  def empty[A](dim: Int)(implicit A: LinAlg[A]): VPolytopeM[A] =
    apply(dim, Seq.empty, Seq.empty)
  
  def fromRays[A](allRayPoints: Mat[A], symGroup: Grp[Perm] = Grp.trivial[Perm])(implicit A: LinAlg[A]): VPolytopeM[A] = {
    import A.IMat
    val newGrp = Grp.fromGeneratorsAndOrder(symGroup.generators.map((Perm.id: Perm, _)), symGroup.order) // TODO: remove :Perm cast after new Alasc release
    new VPolytopeM(zeros[A](0, allRayPoints.nCols), allRayPoints, newGrp)
  }

  def fromVertices[A](allVertexPoints: Mat[A], symGroup: Grp[Perm] = Grp.trivial[Perm])(implicit A: LinAlg[A]): VPolytopeM[A] = {
    import A.IMat
    val newGrp = Grp.fromGeneratorsAndOrder(symGroup.generators.map((_, Perm.id: Perm)), symGroup.order)
    new VPolytopeM(allVertexPoints, zeros[A](0, allVertexPoints.nCols), newGrp)
  }

  object WithoutSym {

    def union[A](lhs: VPolytopeM[A], rhs: VPolytopeM[A])(implicit A: LinAlg[A]): VPolytopeM[A] = {
      import A.IMat
      val mV = lhs.mV.vertcat(rhs.mV)
      val mR = lhs.mR.vertcat(rhs.mR)
      VPolytopeM(mV, mR)
    }

  }

}
