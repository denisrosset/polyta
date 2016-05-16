package com.faacets.polyta

import spire.algebra.Order
import spire.util.Opt

/** Extended scalar value that could be +/- infinity. */
sealed abstract class Extended[A] {

  /** The catamorphism (nice word!) for the extended scalar. */
  def fold[X](
    minusInf: => X,
    finite: A => X,
    plusInf: => X
  ): X = this match {
    case Extended.MinusInf() => minusInf
    case Extended.Finite(a) => finite(a)
    case Extended.PlusInf() => plusInf
  }

  /** Returns true if this extended scalar is -infinity. */
  def isMinusInf: Boolean

  /** Returns true if this extended scalar is +infinity. */
  def isPlusInf: Boolean

  /** Returns true if this extended scalar is +/-infinity. */
  def isInf: Boolean

  /** Returns true if this extended scalar is finite. */
  def isFinite: Boolean

  /** Returns an `Opt` containing this finite scalar value or `Opt.empty[A]`. */
  def finite: Opt[A]

}

trait ExtendedOrder[A] extends Any with Order[Extended[A]] {
  implicit def A: Order[A]
  def compare(x: Extended[A], y: Extended[A]): Int = x match {
    case _: Extended.MinusInf[A] => y match {
      case _: Extended.MinusInf[A] => 0
      case _ => -1
    }
    case Extended.Finite(ax) => y match {
      case _: Extended.MinusInf[A] => 1
      case Extended.Finite(ay) => A.compare(ax, ay)
      case _: Extended.PlusInf[A] => -1
    }
    case _: Extended.PlusInf[A] => y match {
      case _: Extended.PlusInf[A] => 0
      case _ => 1
    }
  }
}

object Extended {

  case class MinusInf[A]() extends Extended[A] {
    def isMinusInf = true
    def isPlusInf = false
    def isInf = true
    def isFinite = false
    def finite: Opt[A] = Opt.empty[A]
  }

  case class Finite[A](a: A) extends Extended[A] {
    def isMinusInf = false
    def isPlusInf = false
    def isInf = false
    def isFinite = true
    def finite: Opt[A] = Opt(a)
  }


  case class PlusInf[A]() extends Extended[A] {
    def isMinusInf = false
    def isPlusInf = true
    def isInf = true
    def isFinite = false
    def finite: Opt[A] = Opt.empty[A]
  }

  implicit def order[A:Order]: Order[Extended[A]] = new ExtendedOrder[A] {
    def A = Order[A]
  }

}
