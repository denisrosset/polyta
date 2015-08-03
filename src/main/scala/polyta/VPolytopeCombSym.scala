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

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._
import net.alasc.util._

// TODO: split into vertices/rays and allVertices/allRays
final class VPolytopeCombSym[V, @sp(Double, Long) A](val nX: Int, val vertexPoints: Seq[V], val rayPoints: Seq[V], val symGroup: Grp[(Perm, Perm)])(implicit val pack: PackField.ForV[V, A], val orderA: Order[A]) extends VPolytope[V, A] {
  val nVertices = vertexPoints.size
  val nRays = rayPoints.size
  object representation extends Representation[G] {
    def represents(g: G): Boolean = g._1.supportMax.getOrElse(-1) < nVertices && g._2.supportMax.getOrElse(-1) < nRays
    def size = nVertices + nRays
    def representations = Opt.empty
    object action extends FaithfulPermutationAction[G] {
      def actr(i: Int, g: G): Int =
        if (i < nVertices) i <|+| g._1 else ((i - nVertices) <|+| g._2) + nVertices
      def actl(g: G, i: Int) =
        if (i < nVertices) g._1 |+|> i else (g._2 |+|> (i - nVertices)) + nVertices
      def support(g: G): Set[Int] = g._1.support ++ (g._2.support.map(_ + nVertices))
      def supportMax(g: G): NNOption = g._2.supportMax match {
        case NNOption(mx) => NNSome(mx + nVertices)
        case _ => g._1.supportMax
      }
      def supportMin(g: G): NNOption = g._1.supportMin match {
        case NNOption(mn) => NNSome(mn)
        case _ => g._2.supportMin match {
          case NNOption(mn) => NNSome(mn + nVertices)
          case _ => NNNone
        }
      }
      def supportMaxElement = size - 1
    }
  }
  def vertices = vertexPoints.indices.map(new Vertex(_))
  def rays = rayPoints.indices.map(new Ray(_))
  type G = (Perm, Perm) // vertex permutation, ray permutation
  sealed trait Element extends ElementBase[V]
  final class Vertex(val index: Int) extends Element with VertexBase[V] {
    def point: V = vertexPoints(index)
    def representatives: Iterable[Vertex] = {
      import net.alasc.math.OrbitInstances._
      val orbit = Set(index) <|+| symGroup.generators.map(_._1)
      orbit.map( new Vertex(_) )
    }
  }
  final class Ray(val index: Int) extends Element with RayBase[V] {
    def point: V = rayPoints(index)
    def representatives: Iterable[Ray] = {
      import net.alasc.math.OrbitInstances._
      val orbit = Set(index) <|+| symGroup.generators.map(_._2)
      orbit.map( new Ray(_) )
    }
  }
  def symSubgroup(e: Element): Grp[G] = e match {
    case v: Vertex => symGroup.stabilizer(v.index, representation)._1
    case r: Ray => symGroup.stabilizer(vertexPoints.size + r.index, representation)._1
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
    def actr(v: Vertex, g: G): Vertex = new Vertex(v.index <|+| g._1)
    def actl(g: G, v: Vertex): Vertex = new Vertex(g._1 |+|> v.index)
  }
  implicit object rayAction extends Action[Ray, G] {
    def actr(r: Ray, g: G): Ray = new Ray(r.index <|+| g._2)
    def actl(g: G, r: Ray): Ray = new Ray(g._2 |+|> r.index)
  }
}
