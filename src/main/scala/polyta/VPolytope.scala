package com.faacets
package polyta

import scala.{specialized => sp}
import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.math.Rational
import spire.std.tuples._
import spire.syntax.action._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.prep.PGrp.default._
import net.alasc.perms.Perm

import scalin.immutable.{Mat, Vec}

import VPolytope.PermPermPRepBuilder
/*
final class SingleVertex[V](val point: V) extends VertexBase[V] {
  type VX = SingleVertex[V]
  def representatives = Iterable(this)
}

final class SingleRay[V](val point: V) extends RayBase[V] {
  type R = SingleRay[V]
  def representatives = Iterable(this)
}
 */

/** Polytope described by extremal vertices. */

trait VPolytope[A] extends Polytope[A] { lhs =>

  type G

  type Element <: VPolytope.Element.ForG[A, G]
  type Vertex <: Element with VPolytope.Vertex.ForG[A, G] { type E = Vertex }
  type Ray <: Element with VPolytope.Ray.ForG[A, G] { type E = Ray }

  /** Vertex representatives, one for each orbit. */
  def vertices: Seq[Vertex]

  /** All vertices. */
  def allVertices: Seq[Vertex] = vertices.flatMap(_.representatives)

  /** Ray representatives, one for each orbit. */
  def rays: Seq[Ray]

  /** All rays. */
  def allRays: Seq[Ray] = rays.flatMap(_.representatives)

  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "\nRays:\n" + rays.mkString("\n")

  /** Action of the symmetry group on vertices and rays. */
  implicit def elementAction: Action[Element, G]

  /** Action of the symmetry group on vertices. */
  implicit def vertexAction: Action[Vertex, G]

  /** Action of the symmetry group on rays. */
  implicit def rayAction: Action[Ray, G]

  def symmetriesDiscarded: VPolytope[A] = {
    val allVertexPoints: Seq[Vec[A]] = allVertices.map(_.point)
    val allRayPoints: Seq[Vec[A]] = allRays.map(_.point)
    VPolytope(dim, allVertexPoints, allRayPoints)
  }

/*  def equalities: Seq[LinearEquality[V, A]] = {
    val headV = vertices.head.point
    val otherV = vertices.tail.map(_.point)
    val basis = otherV.map(_ - headV).orthogonalComplement(nX)
    basis.map( vec => LinearEquality(vec, vec.dot(headV)) )
  }*/

  /*
  /** TODO: what happens with rays ? */
  def facetOn(onVertices: Seq[Vertex], satisfying: Vertex): LinearInequality[V, A] = {
    implicit def A: Field[A] = pack.A
    val zeroH = onVertices.head.point
    val zeroT = onVertices.tail.map(_.point)
    val zeroVertices = zeroT.map(_ - zeroH)
    val nonZeroVertex = satisfying.point - zeroH
    val lhs = nonZeroVertex.orthogonalized(zeroVertices: _*)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequality(lhs, LE,  rhs)
    else
      LinearInequality(-lhs, LE, -rhs)
  }


*/
}


object VPolytope {

  type ForG[A, G0] = VPolytope[A] { type G = G0 }

  trait Element[A] {
    def point: Vec[A]
    override def toString = point.toString
    /** Symmetry group element type. */
    type G
    /** Self type. */
    type E <: Element.ForG[A, G]
    /** Representatives of this element under symmetry. */
    def representatives: Iterable[E]
    /** Subgroup of the polytope symmetry group leaving this element invariant. */
    def symSubgroup: Grp[G]
  }

  object Element {
    type ForG[A, G0] = Element[A] { type G = G0 }
  }


  trait Vertex[A] extends Element[A] {
    type E <: Vertex.ForG[A, G]
  }

  object Vertex {
    type ForG[A, G0] = Vertex[A] { type G = G0 }
  }

  trait Ray[A] extends Element[A] {
    type E <: Ray.ForG[A, G]
  }

  object Ray {
    type ForG[A, G0] = Ray[A] { type G = G0 }
  }

  def apply[A](vertexPoints: Mat[A], rayPoints: Mat[A])(implicit A: LinAlg[A]): VPolytopeCombSym[A] =
    VPolytopeCombSym[A](vertexPoints, rayPoints)

  def apply[A](dim: Int, vertexPoints: Seq[Vec[A]], rayPoints: Seq[Vec[A]])(implicit A: LinAlg[A]): VPolytopeCombSym[A] = {
    import A.{fieldA, IMat, IVec}
    val vertexM = IMat.tabulate(vertexPoints.size, dim)( (r, c) => vertexPoints(r)(c) )
    val rayM = IMat.tabulate(rayPoints.size, dim)( (r, c) => rayPoints(r)(c) )
    apply(vertexM, rayM)
  }

  implicit object PermPermPRepBuilder extends PRepBuilder[(Perm, Perm)] {

    type R = PermPermPRep

    def classTagR = classTag[PermPermPRep]

    case class PermPermPRep(size1: Int, size2: Int) extends BuiltRep[(Perm, Perm)] with FaithfulPRep[(Perm, Perm)] {
      type B = PermPermPRepBuilder.this.type
      val builder: B = PermPermPRepBuilder.this
      def size = size1 + size2
      def represents(pair: (Perm, Perm)) = pair._1.supportMax.getOrElse(-1) < size1 && pair._2.supportMax.getOrElse(-1) < size2
      object permutationAction extends FaithfulPermutationAction[(Perm, Perm)] {
        def actr(p: Int, pair: (Perm, Perm)): Int =
          if (p < size1) pair._1.image(p)
          else if (p < size1 + size2) pair._2.image(p - size1)
          else p
        def actl(pair: (Perm, Perm), p: Int): Int =
          if (p < size1) pair._1.invImage(p)
          else if (p < size + size2) pair._2.invImage(p - size2)
          else p
        def support(pair: (Perm, Perm)) = pair._1.support ++ pair._2.support.map(_ + size1)
        def supportMin(pair: (Perm, Perm)) = pair._1.supportMin.orElseInt(pair._2.supportMin.mapInt(_ + size1))
        def supportMax(pair: (Perm, Perm)) = pair._2.supportMax.mapInt(_ + size1).orElseInt(pair._1.supportMax)
        def supportMaxElement = size1 + size2
      }
    }

    def build(generators: Iterable[(Perm, Perm)]): PermPermPRep = {
      val size1 = generators.foldLeft(-1) { (mx, pair) => spire.math.min(pair._1.supportMax.getOrElse(-1), mx) }
      val size2 = generators.foldLeft(-1) { (mx, pair) => spire.math.min(pair._2.supportMax.getOrElse(-1), mx) }
      PermPermPRep(size1, size2)
    }

    object lattice extends Lattice[PermPermPRep] with BoundedJoinSemilattice[PermPermPRep] {
      def zero = PermPermPRep(0, 0)
      def join(lhs: PermPermPRep, rhs: PermPermPRep): PermPermPRep =
        PermPermPRep(spire.math.max(lhs.size1, rhs.size1), spire.math.max(lhs.size2, rhs.size2))
      def meet(lhs: PermPermPRep, rhs: PermPermPRep): PermPermPRep =
        PermPermPRep(spire.math.min(lhs.size1, rhs.size1), spire.math.min(lhs.size2, rhs.size2))
    }

    object partialOrder extends PartialOrder[PermPermPRep] {
      def partialCompare(lhs: PermPermPRep, rhs: PermPermPRep): Double = {
        val c1 = spire.std.int.IntAlgebra.compare(lhs.size1, rhs.size1)
        val c2 = spire.std.int.IntAlgebra.compare(lhs.size2, rhs.size2)
        if (c1 < 0 && c2 < 0) -1.0
        else if (c1 > 0 && c2 > 0) 1.0
        else if (c1 == 0 && c2 == 0) 0.0
        else Double.NaN
      }
    }

  }

}
