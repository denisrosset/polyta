package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import net.alasc.algebra._
import net.alasc.prep._
import net.alasc.util._

/* TODO: here we cheat and assume that the representation given by the action of both action1 and action2 is faithful. */
class ProductRepresentation[G](val size1: Int, val action1: PermutationAction[G], val size2: Int, val action2: PermutationAction[G]) extends FaithfulPRep[G] {
  def size = size1 + size2
  def represents(g: G): Boolean =
    (action1.supportMax(g).getOrElse(-1) < size1) && (action2.supportMax(g).getOrElse(-1) < size2)
  def representations = Opt.empty
  object action extends FaithfulPermutationAction[G] {
    def actr(i: Int, g: G): Int =
      if (i < size1) action1.actr(i, g) else action2.actr(i - size1, g) + size1
    def actl(g: G, i: Int) =
      if (i < size1) action1.actl(g, i) else action2.actl(g, i - size1) + size1
    def support(g: G): Set[Int] = action1.support(g) ++ action2.support(g).map(_ + size1)
    def supportMax(g: G): NNOption = action2.supportMax(g) match {
      case NNOption(mx) => NNSome(mx + size1)
      case _ => action1.supportMax(g)
    }
    def supportMin(g: G): NNOption = action1.supportMin(g) match {
      case NNOption(mn) => NNSome(mn)
      case _ => action2.supportMin(g) match {
        case NNOption(mn) => NNSome(mn + size1)
        case _ => NNNone
      }
    }
    def supportMaxElement = size - 1
  }
}
