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
import net.alasc.std.any._

trait SymVPolyhedronZS[M, V, @sp(Double) A] extends SymVPolyhedron[V, A] with VPolyhedronZS[M, V, A] {
  implicit def alg: AlgMVF[M, V, A]
  implicit def orderA: Order[A]

  type ZeroSet = Set[Int]

  def hPolyhedron: SymHPolyhedron[V, A]

  def inequalitySymmetryGroup = hPolyhedron.symmetryGroup

  lazy val symmetryGroup = {
    val zsSeq = vertexZeroSets
    val zsMap = zsSeq.zipWithIndex.toMap
    val generators = inequalitySymmetryGroup.generators.map { g =>
      Perm.fromImages(zsSeq.map( zs => zsMap(zs <|+| g) ))
    }
    Grp.fromGenerators(generators.map(g => (g, Perm.Algebra.id) ))
  }

  implicit val enumerable: EnumerableOrdered[ZeroSet, Boolean] = EnumerableOrdered.setInt[Set[Int]](hPolyhedron.inequalities.size)
  implicit val permutable: Permutable[ZeroSet, Perm] = Permutable.setInt(PermutationRepresentations[Perm].forSize(hPolyhedron.inequalities.size))

  def vertexFamilyZeroSets: Seq[ZeroSet]
  def families: Seq[BigIndexedSeq[V]] = new IndexedSeq[BigIndexedSeq[V]] {
    def length = vertexFamilyZeroSets.length
    def apply(idx: Int): BigIndexedSeq[V] = family(vertexFamilyZeroSets(idx))
  }

  def vertexZeroSets: Seq[ZeroSet] = vertexFamilyZeroSets.flatMap( zs => Representatives.ordered(zs, inequalitySymmetryGroup).map { x: RepresentativeOrdered[ZeroSet, Perm] => x.get }.iterator.toSeq )

  def family(representative: ZeroSet) = Representatives.ordered(representative, inequalitySymmetryGroup).map {
    zs: RepresentativeOrdered[ZeroSet, Perm] => VZeroSet[M, V, A](hPolyhedron, zs.get).vertex
  }

  override def nX: Int = hPolyhedron.nX
}

object SymVPolyhedronZS {
  protected def build[M, V, @sp(Double, Long) A](hPolyhedron0: SymHPolyhedron[V, A], vertexFamilyZeroSets0: Seq[Set[Int]])(implicit alg0: AlgMVF[M, V, A], orderA0: Order[A]): SymVPolyhedronZS[M, V, A] = new SymVPolyhedronZS[M, V, A] {
    def alg = alg0
    def orderA = orderA0
    def hPolyhedron = hPolyhedron0
    def vertexFamilyZeroSets = vertexFamilyZeroSets0
  }

  def apply[M, V, @sp(Double, Long) A: Order](hPolyhedron: SymHPolyhedron[V, A], vertexFamilyZeroSets: Seq[Set[Int]])(implicit MT: MatType[A, V, M], alg: AlgMVF[M, V, A]): SymVPolyhedronZS[M, V, A] = build(hPolyhedron, vertexFamilyZeroSets)

  def fromDualDescription[M, V, @sp(Double, Long) A: Order](vPolyhedron: VPolyhedron[V, A], hPolyhedron: SymHPolyhedron[V, A])(implicit MT: MatType[A, V, M], alg: AlgMVF[M, V, A]): SymVPolyhedronZS[M, V, A] = {
    implicit val enumerable: EnumerableOrdered[Set[Int], Boolean] = EnumerableOrdered.setInt[Set[Int]](hPolyhedron.inequalities.size)
    implicit val permutable: Permutable[Set[Int], Perm] = Permutable.setInt(PermutationRepresentations[Perm].forSize(hPolyhedron.inequalities.size))
    val inequalitySymmetryGroup = hPolyhedron.symmetryGroup
    val zeroSets = vPolyhedron.vertices.map { vertex =>
      val zeroSet = hPolyhedron.inequalities.indices.filter { k =>
        val inequality = hPolyhedron.inequalities(k)
        inequality.lhs.dot(vertex) === inequality.rhs
      }.toSet
      Representatives.ordered(zeroSet, inequalitySymmetryGroup).head.get
    }.toSet.toSeq
    apply(hPolyhedron, zeroSets)
  }
}
