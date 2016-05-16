package com.faacets.polyta

import spire.algebra.{Eq, Group}
import spire.math.Rational

import scalin.immutable.Mat

import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms.Perm
import scalin.syntax.all._

sealed trait Symmetry

object Symmetry {

  case object Without extends Symmetry

  case class Combinatorial(group: Grp[Perm]) extends Symmetry

  object Combinatorial {

    import net.alasc.perms.default._

    val trivial = Combinatorial(Grp.trivial[Perm])

  }

  case class Linear(group: Grp[Mat[Rational]]) extends Symmetry

  object Linear {

    import net.alasc.blackbox._

    def trivial(dim: Int): Linear = {
      import scalin.immutable.dense._
      implicit object matGroup extends Group[Mat[Rational]] {
        override def inverse(a: Mat[Rational]): Mat[Rational] = a.inverse
        override def id: Mat[Rational] = eye[Rational](dim)
        override def op(x: Mat[Rational], y: Mat[Rational]): Mat[Rational] = x * y
      }
      implicit object matEq extends Eq[Mat[Rational]] {
        override def eqv(x: Mat[Rational], y: Mat[Rational]): Boolean = x == y
      }
      Linear(Grp.trivial[Mat[Rational]])
    }

  }

  case class Permutation(group: Grp[Perm]) extends Symmetry

  object Permutation {

    import net.alasc.perms.default._

    val trivial = Permutation(Grp.trivial[Perm])

  }

  trait Intersection[S <: Symmetry] {

    def apply(lhs: S, rhs: S): S

  }

  object Intersection {

    implicit object without extends Intersection[Without.type] {

      def apply(lhs: Without.type, rhs: Without.type) = Without

    }

    implicit object combinatorial extends Intersection[Combinatorial] {

      def apply(lhs: Combinatorial, rhs: Combinatorial) = Combinatorial.trivial

    }

    implicit def linear(implicit B: GrpBuilder[Mat[Rational]]): Intersection[Linear] = new Intersection[Linear] {

      def apply(lhs: Linear, rhs: Linear) = Linear(lhs.group.intersect(rhs.group))

    }

    implicit object permutation extends Intersection[Permutation] {

      import net.alasc.perms.default._

      def apply(lhs: Permutation, rhs: Permutation) = Permutation(lhs.group.intersect(rhs.group))

    }

  }

}
