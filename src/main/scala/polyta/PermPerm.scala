package com.faacets
package polyta

import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.perms.Perm

object PermPerm {

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
      val size1 = generators.foldLeft(-1) { (mx, pair) => spire.math.max(pair._1.supportMax.getOrElse(-1), mx) }
      val size2 = generators.foldLeft(-1) { (mx, pair) => spire.math.max(pair._2.supportMax.getOrElse(-1), mx) }
      PermPermPRep(size1 + 1, size2 + 1)
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
