package com.faacets
package polyta

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._

import net.alasc.algebra._
import net.alasc.math.Grp

trait SymHPolyhedron[G, M, V, @sp(Double) A] extends HPolyhedron[M, V, A] {
  def eqAction: PermutationAction[G]
  def ineqAction: PermutationAction[G]
  def symmetryGroup: Grp[G]
}

trait SymVPolyhedron[G, M, V, @sp(Double) A] extends VPolyhedron[M, V, A] {
  def vertexAction: PermutationAction[G]
  def rayAction: PermutationAction[G]
  def symmetryGroup: Grp[G]
}
