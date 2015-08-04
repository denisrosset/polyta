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
import qalg.syntax.indup.all._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._
import net.alasc.util._

final class VPolytopeCombSym[M, V, @sp(Double, Long) A, G0](
  val allVertexPoints: M,
  val allRayPoints: M,
  val symGroup: Grp[G0],
  val vertexIndexAction: PermutationAction[G0], // TODO: support for non-faithful representations
  val rayIndexAction: PermutationAction[G0],
  val vertexOrbitRepresentatives: Seq[Int],
  val rayOrbitRepresentatives: Seq[Int]
)(
  implicit val pack: PackField.ForMV[M, V, A],
  val orderA: Order[A]
) extends VPolytope[V, A] {

  type G = G0

  val nX = allVertexPoints.nCols

  val totalVertices = allVertexPoints.nRows
  val totalRays = allRayPoints.nRows

  val representation = new ProductRepresentation(totalVertices, vertexIndexAction, totalRays, rayIndexAction)

  val vertices = vertexOrbitRepresentatives.view.map(new Vertex(_))
  val rays = rayOrbitRepresentatives.view.map(new Ray(_))

  override val allVertices = (0 until allVertexPoints.nRows).view.map(new Vertex(_))
  override val allRays = (0 until allRayPoints.nRows).view.map(new Ray(_))

  sealed trait Element extends ElementBase[V, G]
  final class Vertex(val index: Int) extends Element with VertexBase[V, G] {
    type E = Vertex
    def point: V = allVertexPoints(index, ::)
    def representatives: Iterable[Vertex] = {
      val orbit = Orbits.orbit(index, symGroup.generators, vertexIndexAction)
      orbit.map( new Vertex(_) )
    }
    def symSubgroup: Grp[G] = symGroup.stabilizer(index, representation)._1
  }
  final class Ray(val index: Int) extends Element with RayBase[V, G] {
    type E = Ray
    def point: V = allRayPoints(index, ::)
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
//  def apply[M, V, A: Order](vertexPoints: M, rayPoints: M)(implicit pack: PackField.ForMV[M, V, A]): VPolytopeCombSym[M, V, A] = new VPolytopeCombSym(vertexPoints, rayPoints, Grp.trivial[(Perm, Perm)], 0 until vertexPoints.nRows, 0 until rayPoints.nRows)
}
