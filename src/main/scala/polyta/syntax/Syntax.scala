package com.faacets.polyta
package syntax

import solvers._

import scala.language.implicitConversions

trait PolyhedronSyntax {
  implicit def hDualOps[P <: HPolyhedron[_, _], D](lhs: P)(implicit ev: Dual[P, D]): DualOps[P, D] = new DualOps[P, D](lhs)
  implicit def vDualOps[P <: VPolyhedron[_, _], D](lhs: P)(implicit ev: Dual[P, D]): DualOps[P, D] = new DualOps[P, D](lhs)
  implicit def hReducedDualOps[P <: HPolyhedron[_, _], R](lhs: P)(implicit ev: ReducedDual[P, R]): ReducedDualOps[P, R] = new ReducedDualOps[P, R](lhs)
  implicit def vReducedDualOps[P <: VPolyhedron[_, _], R](lhs: P)(implicit ev: ReducedDual[P, R]): ReducedDualOps[P, R] = new ReducedDualOps[P, R](lhs)
  implicit def hSymmetric[P <: HPolyhedron[_, _], S](lhs: P)(implicit ev: Symmetric[P, S]): SymmetricOps[P, S] = new SymmetricOps[P, S](lhs)
  implicit def vSymmetric[P <: VPolyhedron[_, _], S](lhs: P)(implicit ev: Symmetric[P, S]): SymmetricOps[P, S] = new SymmetricOps[P, S](lhs)
}

trait
  AllSyntax extends PolyhedronSyntax
