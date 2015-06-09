package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.math.enum._
import net.alasc.std.set._

trait SymHPolyhedronZS[M, V, @sp(Double) A] extends SymHPolyhedron[V, A] with HPolyhedronZS[M, V, A] {
  implicit def alg: AlgMVF[M, V, A]
  implicit def orderA: Order[A]

  type ZeroSet = Set[Int]

  def vPolyhedron: SymVPolyhedron[V, A]

  require(vPolyhedron.rays.isEmpty)

  val vertexSymmetryGroup = Grp.fromGenerators(vPolyhedron.symmetryGroup.generators.map(_._1))

  lazy val symmetryGroup = {
    val zsSeq = inequalityZeroSets
    val zsMap = zsSeq.zipWithIndex.toMap
    val generators = vertexSymmetryGroup.generators.map { g =>
      Perm.fromImages(zsSeq.map( zs => zsMap(zs <|+| g) ))
    }
    Grp.fromGenerators(generators)
  }

  implicit val enumerable: EnumerableOrdered[ZeroSet, Boolean] = EnumerableOrdered.setInt[Set[Int]](vPolyhedron.vertices.size)
  implicit val permutable: Permutable[ZeroSet, Perm] = Permutable.setInt(PermutationRepresentations[Perm].forSize(vPolyhedron.vertices.size))

  def inequalityFamilyZeroSets: Seq[ZeroSet]
  def families: Seq[BigIndexedSeq[LinearInequality[V, A]]] = new IndexedSeq[BigIndexedSeq[LinearInequality[V, A]]] {
    def length = inequalityFamilyZeroSets.length
    def apply(idx: Int): BigIndexedSeq[LinearInequality[V, A]] = family(inequalityFamilyZeroSets(idx))
  }

  def inequalityZeroSets: Seq[ZeroSet] = inequalityFamilyZeroSets.flatMap( zs => Representatives.ordered(zs, vertexSymmetryGroup).map { x: RepresentativeOrdered[ZeroSet, Perm] => x.get }.iterator.toSeq )

  def family(representative: ZeroSet) = Representatives.ordered(representative, vertexSymmetryGroup).map {
    zs: RepresentativeOrdered[ZeroSet, Perm] => HZeroSet[M, V, A](vPolyhedron, zs.get).inequality
  }

  override def nX: Int = vPolyhedron.nX
}

object SymHPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](vPolyhedron0: SymVPolyhedron[V, A], inequalityFamilyZeroSets0: Seq[Set[Int]], equalities0: Seq[LinearEquality[V, A]])(implicit alg0: AlgMVF[M, V, A], orderA0: Order[A]): SymHPolyhedronZS[M, V, A] = new SymHPolyhedronZS[M, V, A] {
    def alg = alg0
    def V = alg0.V
    def orderA = orderA0
    def vPolyhedron = vPolyhedron0
    def inequalityFamilyZeroSets = inequalityFamilyZeroSets0
    def equalities = equalities0
  }

  def apply[M, V, @sp(Double, Long) A: Order](vPolyhedron: SymVPolyhedron[V, A], inequalityFamilyZeroSets: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]])(implicit MT: MatType[A, V, M], alg: AlgMVF[M, V, A]): SymHPolyhedronZS[M, V, A] = build(vPolyhedron, inequalityFamilyZeroSets, equalities)

  def fromDualDescription[M, V, @sp(Double, Long) A: Order](hPolyhedron: HPolyhedron[V, A], vPolyhedron: SymVPolyhedron[V, A])(implicit MT: MatType[A, V, M], alg: AlgMVF[M, V, A]): SymHPolyhedronZS[M, V, A] = {
    implicit val enumerable: EnumerableOrdered[Set[Int], Boolean] = EnumerableOrdered.setInt[Set[Int]](vPolyhedron.vertices.size)
    implicit val permutable: Permutable[Set[Int], Perm] = Permutable.setInt(PermutationRepresentations[Perm].forSize(vPolyhedron.vertices.size))
    val vertexSymmetryGroup = Grp.fromGenerators(vPolyhedron.symmetryGroup.generators.map(_._1))
    val zeroSets = hPolyhedron.inequalities.map { inequality =>
      val zeroSet = vPolyhedron.vertices.indices.filter { k =>
        val vertex = vPolyhedron.vertices(k)
        inequality.lhs.dot(vertex) === inequality.rhs
      }.toSet
      Representatives.ordered(zeroSet, vertexSymmetryGroup).head.get
    }.toSet.toSeq
    apply(vPolyhedron, zeroSets, hPolyhedron.equalities)
  }
}
