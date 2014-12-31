package com.faacets
package polyta

import qalg._
import net.alasc.math.{Grp, Perm}

/** Polytope with rational extremal points.
  */
class Polytope(myHRepr: Option[HRepr], myVRepr: Option[VRepr], name: String = "") {

  assert(!myHRepr.isEmpty || !myVRepr.isEmpty)

  lazy val hRepr: HRepr = if (myHRepr.isEmpty) myVRepr.get.toH(name) else myHRepr.get

  lazy val vRepr: VRepr = if (myVRepr.isEmpty) myHRepr.get.toV(name) else myVRepr.get

  def dim: Integer = if(!myHRepr.isEmpty) myHRepr.get.dim else myVRepr.get.dim

  def allExtremal: qalg.immutable.QMatrix = vRepr.e
  def allRays: qalg.immutable.QMatrix = vRepr.r
  def allInequalities: (qalg.immutable.QMatrix, qalg.immutable.QVector) = (hRepr.a, hRepr.b)
  def allEqualities: (qalg.immutable.QMatrix, qalg.immutable.QVector) = (hRepr.aeq, hRepr.beq)

}

/*
class SymPolytope(mySymHRepr: Option[SymHRepr], mySymVRepr: Option[SymVRepr]) extends Polytope(mySymHRepr, mySymVRepr) {

  override lazy val hRepr: SymHRepr = if (mySymHRepr.isEmpty) mySymVRepr.get.toH else mySymHRepr.get

  override lazy val vRepr: SymVRepr = if (mySymVRepr.isEmpty) mySymHRepr.get.toV else mySymVRepr.get

}
 */

abstract class PolytopeRepr {
  def dim: Integer
  /*  def maximize(coefficients: alg.QVector): Rational = minimize(-coefficients)
 def minimize(coefficients: alg.QVector): Rational*/
}

/** Half-plane representation of a polytope.
  * 
  * @param a     Matrix for inequality constraints.
  * @param b     Bound vector for inequalities such that a * x <= b.
  * @param aeq   Matrix for equality constraints.
  * @param beq   Coefficient vector for equality constraints such that aeq * x = beq.
  * @param valid Valid point satisfying inequality and equality constraints.
  */
class HRepr(
  val a: qalg.immutable.QMatrix, val b: qalg.immutable.QVector,
  val aeq: qalg.immutable.QMatrix, val beq: qalg.immutable.QVector,
  val valid: Option[qalg.immutable.QVector]) extends PolytopeRepr {
  def dim = List(a.cols, aeq.cols).max
  def toV(name: String) = Porta.hToV(this, name)
}

/** Half-plane representation of a polytope, with symmetries.
  * 
  * @param a     Matrix for inequality constraints.
  * @param b     Bound vector for inequalities such that a * x <= b.
  * @param aeq   Matrix for equality constraints.
  * @param beq   Coefficient vector for equality constraints such that aeq * x = beq.
  * @param valid Valid point satisfying inequality and equality constraints.
  * @param sym   Symmetry group acting on rows of a+aeq in that order.
  */
class SymHRepr(
  override val a: qalg.immutable.QMatrix,
  override val b: qalg.immutable.QVector,
  override val aeq: qalg.immutable.QMatrix,
  override val beq: qalg.immutable.QVector,
  val sym: Grp[Perm],
  override val valid: Option[qalg.immutable.QVector]) extends HRepr(a, b, aeq, beq, valid) {
  override def toV(name: String) = Sympol.hToV(this, name)
}

/** Vertex representation of a polytope.
  * 
  * @param e   Matrix of extremal points, stored as row vectors.
  * @param r   Matrix of rays, stored as row vectors.
  */
class VRepr(val e: qalg.immutable.QMatrix, val r: qalg.immutable.QMatrix) extends PolytopeRepr {
  def dim = List(e.cols, r.cols).max
  def toH(name: String) = Porta.vToH(this, name)
}

/** Vertex representation of a polytope, with symmetries.
  * 
  * @param e   Matrix of extremal points, stored as row vectors.
  * @param r   Matrix of rays, stored as row vectors.
  * @param sym Symmetry group acting on rows of e+r in that order.
  */
case class SymVRepr(
  override val e: qalg.immutable.QMatrix,
  override val r: qalg.immutable.QMatrix,
  val sym: Grp[Perm]) extends VRepr(e, r) {

  override def toH(name: String) = Sympol.vToH(this, name)
}
