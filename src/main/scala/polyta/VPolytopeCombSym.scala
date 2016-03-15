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
import net.alasc.perms.Perm
import net.alasc.finite.Grp
import net.alasc.std.unit._
import net.alasc.util._

/** V-representation polytope with (possible) combinatorial symmetry on its vertices.
  * 
  * @param allVertexPoints            Matrix whose rows are vertices of the polytope
  * @param allRayPoints               Matrix whose rows are rays of the polytope
  * @param symGroup                   Symmetry group of the polytope
  * @param vertexIndexAction          Action of symmetry elements on the vertex indices
  * @param rayIndexAction             Action of symmetry elements on the ray indices
  * @param vertexOrbitRepresentatives Indices of the representative vertices under symmetry
  * @param rayOrbitRepresentatives    Indices of the representative rays under symmetry
  */
final class VPolytopeCombSym[A, G0](
  val allVertexPoints: Mat[A],
  val allRayPoints: Mat[A],
  val symGroup: Grp[G0],
  val vertexIndexAction: PermutationAction[G0], // TODO: support for non-faithful representations
  val rayIndexAction: PermutationAction[G0],
  val vertexOrbitRepresentatives: Seq[Int],
  val rayOrbitRepresentatives: Seq[Int]
)(
  implicit val A: LinAlg[A]
) extends VPolytope[A] {

  type G = G0

  def dim = allVertexPoints.nCols

  def nTotalVertices = allVertexPoints.nRows
  def nTotalRays = allRayPoints.nRows

  def representation = new ProductRepresentation(nTotalVertices, vertexIndexAction, nTotalRays, rayIndexAction)

  def vertices = vertexOrbitRepresentatives.view.map(new Vertex(_))

  def rays = rayOrbitRepresentatives.view.map(new Ray(_))

  override def allVertices = (0 until allVertexPoints.nRows).view.map(new Vertex(_))

  override def allRays = (0 until allRayPoints.nRows).view.map(new Ray(_))


  /*def vertexIndexSet(facet: HPolytope.Facet[A]): Set[Int] = {
    val ineq = facet.inequality
    val res = allVertexPoints * ineq.lhs // vertices are sorted as rows
      (0 until res.length).toSet.filter(i => res(i) === ineq.rhs)
  }*/

  sealed trait Element extends VPolytope.Element[A] {

    type G = VPolytopeCombSym.this.G

  }

  final class Vertex(val index: Int) extends Element with VPolytope.Vertex[A] {
    type E = Vertex
    def point: Vec[A] = allVertexPoints(index, ::)
    def representatives: Iterable[Vertex] = {
      val orbit = Orbits.orbit(index, symGroup.generators, vertexIndexAction)
      orbit.map( new Vertex(_) )
    }
    def symSubgroup: Grp[G] = symGroup.stabilizer(index, representation)._1
  }

  final class Ray(val index: Int) extends Element with VPolytope.Ray[A] {
    type E = Ray
    def point: Vec[A] = allRayPoints(index, ::)
    def representatives: Iterable[Ray] = {
      val orbit = Orbits.orbit(index, symGroup.generators, rayIndexAction)
      orbit.map( new Ray(_) )
    }
    def symSubgroup: Grp[G] = symGroup.stabilizer(index + totalVertices, representation)._1
  }

  implicit object elementAction extends Action[Element, G] {
    def actr(e: Element, g: G): Element = e match {
      case v: Vertex => vertexAction.actr(v, g)
      case r: Ray => rayAction.actr(r, g)
    }
    def actl(g: G, e: Element): Element = e match {
      case v: Vertex => vertexAction.actl(g, v)
      case r: Ray => rayAction.actl(g, r)
    }
  }

  implicit object vertexAction extends Action[Vertex, G] {
    def actr(v: Vertex, g: G): Vertex = new Vertex(vertexIndexAction.actr(v.index, g))
    def actl(g: G, v: Vertex): Vertex = new Vertex(vertexIndexAction.actl(g, v.index))
  }

  implicit object rayAction extends Action[Ray, G] {
    def actr(r: Ray, g: G): Ray = new Ray(rayIndexAction.actr(r.index, g))
    def actl(g: G, r: Ray): Ray = new Ray(rayIndexAction.actl(g, r.index))
  }

}
/*
object VPolytopeCombSym {
  import collection.immutable.BitSet

  def apply[M, V, A: Order, G](allVertexPoints: M, allRayPoints: M, symGroup: Grp[G], vertexAction: PermutationAction[G], rayAction: PermutationAction[G])(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A, G] = {
    val totalVertices = allVertexPoints.nRows
    val totalRays = allRayPoints.nRows
    val generators = symGroup.generators
    val vertexOrbits = Orbits.orbits(totalVertices, generators, vertexAction)
    val rayOrbits = Orbits.orbits(totalRays, generators, rayAction)
    val vertexOrbitRepresentatives = vertexOrbits.map(_.head).toSeq.sorted
    val rayOrbitRepresentatives = rayOrbits.map(_.head).toSeq.sorted
    new VPolytopeCombSym(allVertexPoints, allRayPoints, symGroup,
      vertexAction, rayAction,
      vertexOrbitRepresentatives, rayOrbitRepresentatives)
  }

  def apply[M, V, A: Order,
    P1: PermutationRepresentations,
    P2: PermutationRepresentations](
    allVertexPoints: M,
      allRayPoints: M,
      symGroup: Grp[(P1, P2)]
  )(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A, (P1, P2)] = {
    val totalVertices = allVertexPoints.nRows
    val totalRays = allRayPoints.nRows
    val vertexSingleAction = PermutationRepresentations[P1].forSize(totalVertices).action.on[(P1, P2)](_._1)
    val raySingleAction = PermutationRepresentations[P2].forSize(totalRays).action.on[(P1, P2)](_._2)

    apply(allVertexPoints, allRayPoints, symGroup,
      vertexSingleAction, raySingleAction)
  }

  def apply[M, V, A: Order](vertexPoints: M, rayPoints: M)(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A, (Perm, Perm)] = {
    import spire.std.tuples._
    import net.alasc.std.product._
    apply(vertexPoints, rayPoints, Grp.trivial[(Perm, Perm)])
  }
}
 */
