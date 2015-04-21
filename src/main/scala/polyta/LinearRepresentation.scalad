package com.faacets
package polyta

import spire.algebra._
import net.alasc.algebra._
import qalg.algebra._

trait LinearRepresentation[M, G] {
  def apply(g: G): M
}

trait PermutationRepresentation[M, G, A] extends LinearRepresentation[M, G] {
  implicit def M: MatInRing[M, A]
  implicit def A: Ring[A] = M.scalar
  implicit def representation: Representation[G]

  def apply(g: G): M = M.fromFunM(new FunM[A] {
    def nR = representation.size
    def nC = representation.size
    def f(r: Int, c: Int): A =
      if (r == representation.action.actr(c, g)) A.one else A.zero
  })
}
