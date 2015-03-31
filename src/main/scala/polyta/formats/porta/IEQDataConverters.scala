package com.faacets
package polyta
package formats
package porta

import spire.math.Rational
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

trait IEQDataToHPolyhedron[M, V] extends Converter[IEQData[V], HPolyhedron[M, V, Rational]] {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  def convert(from: IEQData[V]): HPolyhedron[M, V, Rational] = {
    assert(from.lowerBounds.isEmpty)
    assert(from.upperBounds.isEmpty)
    val (eqs, ineqs) = from.constraints.partition(_.op == EQ)
    val ineqRows = M.zeros(0, from.dim) +: ineqs.map {
      case VecConstraint(vec, LE, _) => vec.rowMat[M]
      case VecConstraint(vec, GE, _) => (-vec).rowMat[M]
    }
    val mA = M.vertcat(ineqRows: _*)
    val vb = V.build(ineqs.map {
      case VecConstraint(_, LE, r) => r
      case VecConstraint(_, GE, r) => -r
    }: _*)
    val eqRows = M.zeros(0, from.dim) +: eqs.map( c => c.lhs.rowMat[M] )
    val mAeq = M.vertcat(eqRows: _*)
    val vbeq = V.build(eqs.map(_.rhs): _*)
    HPolyhedron(mA, vb, mAeq, vbeq)
  }
}

trait IEQDataFromVPolyhedron[M, V] extends Converter[HPolyhedron[M, V, Rational], IEQData[V]] {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  def convert(from: HPolyhedron[M, V, Rational]): IEQData[V] = {
    val eqs = (0 until from.nEqs).map { r => VecConstraint(from.mAeq(r, ::), EQ, from.vbeq(r)) }
    val ineqs = (0 until from.nIneqs).map { r => VecConstraint(from.mA(r, ::), LE, from.vb(r)) }
    IEQData[V](from.nX, eqs ++ ineqs)
  }
}
