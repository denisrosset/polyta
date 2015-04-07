package com.faacets
package polyta

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._

import net.alasc.algebra._
import net.alasc.math.Grp

trait Family[G, V, A] {
  def lhs: V
  def rhs: A
  def representatives: IndexedSeq[V]
  def symmetrySubgroup: Grp[G]
}

trait SymHPolyhedron[R <: LinearRepresentation[M, G], G, M, V, @sp(Double) A] extends HPolyhedron[M, V, A] {
  def actionX: R
  def symmetryGroup: Grp[G]

  def eqFamilies: Seq[Family[G, V, A]]
  def ineqFamilies: Seq[Family[G, V, A]]
}
