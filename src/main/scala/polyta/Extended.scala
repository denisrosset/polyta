package com.faacets
package polyta

import spire.algebra.Order
import spire.math.Rational

/** Extended scalar value that could be +/- infinity. */
sealed trait Extended[A]
case class MinusInf[A]() extends Extended[A]
case class Scalar[A](a: A) extends Extended[A]
case class PlusInf[A]() extends Extended[A]

trait ExtendedOrder[A] extends Any with Order[Extended[A]] {
  implicit def A: Order[A]
  def compare(x: Extended[A], y: Extended[A]): Int = x match {
    case _: MinusInf[A] => y match {
      case _: MinusInf[A] => 0
      case _ => -1
    }
    case Scalar(ax) => y match {
      case _: MinusInf[A] => 1
      case Scalar(ay) => A.compare(ax, ay)
      case _: PlusInf[A] => -1
    }
    case _: PlusInf[A] => y match {
      case _: PlusInf[A] => 0
      case _ => 1
    }
  }
}

trait ExtendedInstances {
  implicit def extended[A: Order]: Order[Extended[A]] =
    new ExtendedOrder[A] {
      def A = Order[A]
    }
}

object Extended extends ExtendedInstances {
  implicit val extendedRational: Order[Extended[Rational]] =
    new ExtendedOrder[Rational] {
      def A = Order[Rational]
    }
}
