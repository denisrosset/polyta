package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.std.tuples._
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

import VPolytope.PermPermPRepBuilder

/** V-representation polytope with (possible) combinatorial symmetry on its vertices.
  * 
  * @param allVertexPoints            Matrix whose rows are vertices of the polytope
  * @param allRayPoints               Matrix whose rows are rays of the polytope
  * @param symGroup                   Symmetry group of the polytope
  * @param vertexOrbitRepresentatives Indices of the representative vertices under symmetry
  * @param rayOrbitRepresentatives    Indices of the representative rays under symmetry
  */
final class VPolytopeCombSym[A](
  val allVertexPoints: Mat[A],
  val allRayPoints: Mat[A],
  val symGroup: Grp[(Perm, Perm)]
)(
  implicit val A: LinAlg[A]
) extends VPolytope[A] {

  type G = (Perm, Perm)

  def representation = PermPermPRepBuilder.PermPermPRep(allVertexPoints.nRows, allRayPoints.nRows)

  def dim = allVertexPoints.nCols

  def nTotalVertices = allVertexPoints.nRows
  def nTotalRays = allRayPoints.nRows

  def vertices = vertexOrbitRepresentatives.view.map(new Vertex(_))

  def rays = rayOrbitRepresentatives.view.map(new Ray(_))

  override def allVertices = (0 until allVertexPoints.nRows).view.map(new Vertex(_))

  override def allRays = (0 until allRayPoints.nRows).view.map(new Ray(_))

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
    val res = allVertexPoints * ineq.lhs // vertices are sorted as rows
    (0 until res.length).toSet.filter(i => res(i) === ineq.rhs)
  }

  sealed trait Element extends VPolytope.Element[A] {

    type G = (Perm, Perm)

  }

  final class Vertex(val index: Int) extends Element with VPolytope.Vertex[A] {
    import A.IVec
    type E = Vertex
    def point: Vec[A] = allVertexPoints(index, ::)
    def representatives: Iterable[Vertex] = {
      val orbit = Orbits.orbit(index, symGroup.generators.map(_._1), Perm.permutation)
      orbit.map( new Vertex(_) )
    }
    def symSubgroup: Grp[G] = symGroup.in(representation).stabilizer(index)
  }

  final class Ray(val index: Int) extends Element with VPolytope.Ray[A] {
    import A.IVec
    type E = Ray
    def point: Vec[A] = allRayPoints(index, ::)
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

object VPolytopeCombSym {

  def apply[A](allVertexPoints: Mat[A], allRayPoints: Mat[A], symGroup: Grp[(Perm, Perm)] = Grp.trivial[(Perm, Perm)])(implicit A: LinAlg[A]): VPolytopeCombSym[A] =
    new VPolytopeCombSym(allVertexPoints, allRayPoints, symGroup)

}
