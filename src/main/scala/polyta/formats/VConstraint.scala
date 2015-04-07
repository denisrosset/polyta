package com.faacets
package polyta
package formats

import scala.{specialized => sp}

import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

case class VConstraint[V, A](lhs: V, op: ComparisonOperator, rhs: A)(implicit val V: VecInField[V, A])

object VConstraint {
  def toHPolyhedron[M, V, @sp(Double) A](dim: Int, constraints: Seq[VConstraint[V, A]])(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    import M.{V, scalar}
    val (eqs, ineqs) = constraints.partition(_.op == EQ)
    val eqRows = M.zeros(0, dim) +: eqs.map(_.lhs.rowMat[M])
    val mAeq = M.vertcat(eqRows: _*)
    val vbeq = V.build(eqs.map(_.rhs): _*)
    val ineqRows = M.zeros(0, dim) +: ineqs.map {
      case VConstraint(vec, LE, _) => vec.rowMat[M]
      case VConstraint(vec, GE, _) => (-vec).rowMat[M]
    }
    val mA = M.vertcat(ineqRows: _*)
    val vb = V.build(ineqs.map {
      case VConstraint(_, LE, r) => r
      case VConstraint(_, GE, r) => -r
    }: _*)
    HPolyhedron(mA, vb, mAeq, vbeq)
  }
}
