package com.faacets.polyta

import scala.collection.mutable.ArrayBuffer

import spire.algebra._
import spire.std.map._
import spire.syntax.vectorSpace._

import com.faacets.polyta.Direction._
/*
class LinearProgramBuilder[A](implicit val A: LinAlg[A]) { self =>

  import A.fieldA

  val varNames = ArrayBuffer.empty[String]

  implicit object Var {
    def apply(name: String): LinExpr = {
      varNames += name
      val index = varNames.size - 1
      LinExpr(Map(index -> Ring[A].one), Ring[A].zero)
    }
  }

  sealed trait ConstraintType
  case object EQ extends ConstraintType
  case object LE extends ConstraintType
  case object GE extends ConstraintType

  case class Constraint(lhs: Map[Int, A], constraintType: ConstraintType, rhs: A)

  case class LinExpr(coeffs: Map[Int, A], scalar: A) {
    def +(a: A): LinExpr = LinExpr(coeffs, scalar + a)
    def -(a: A): LinExpr = LinExpr(coeffs, scalar - a)
    def <=(rhs: A): Constraint = Constraint(coeffs, LE, rhs - scalar)
    def >=(rhs: A): Constraint = Constraint(coeffs, GE, rhs - scalar)
    def :=(rhs: A): Constraint = Constraint(coeffs, EQ, rhs - scalar)
    def <=(rhs: LinExpr): Constraint = (this - rhs) <= Ring[A].zero
    def >=(rhs: LinExpr): Constraint = (this - rhs) >= Ring[A].zero
    def :=(rhs: LinExpr): Constraint = (this - rhs) := Ring[A].zero
  }

  object LinExpr {
    def apply(a: A): LinExpr = LinExpr(Map.empty[Int, A], a)
  }

  implicit val LinExprAlgebra: VectorSpace[LinExpr, A] = new VectorSpace[LinExpr, A] {
    def scalar = fieldA
    implicit val MVS: VectorSpace[Map[Int, A], A] = MapVectorSpace[Int, A](fieldA)
    def zero: LinExpr = LinExpr(Map.empty[Int, A], Ring[A].zero)
    def plus(x: LinExpr, y: LinExpr): LinExpr = LinExpr(MVS.plus(x.coeffs, y.coeffs), x.scalar + y.scalar)
    override def minus(x: LinExpr, y: LinExpr): LinExpr = LinExpr(MVS.minus(x.coeffs, y.coeffs), x.scalar - y.scalar)
    def negate(x: LinExpr): LinExpr = LinExpr(MVS.negate(x.coeffs), -x.scalar)
    def timesl(a: A, x: LinExpr): LinExpr = LinExpr(MVS.timesl(a, x.coeffs), a * x.scalar)
  }

  val constraints = ArrayBuffer.empty[Constraint]
  var objective: Option[(Direction, LinExpr)] = Option.empty

  def subjectTo(newConstraints: Constraint*): Unit =
    constraints ++= newConstraints
  def maximize(function: LinExpr): Unit = {
    objective = Some((Max, function))
  }
  def minimize(function: LinExpr): Unit = {
    objective = Some((Min, function))
  }
  def result: LinearProgram[A] = {
    val dim = varNames.size
    val (eqs, ineqs) = constraints.partition(_.constraintType == EQ)
    val vb = A.IVec.tabulate(ineqs.size) { k =>
      if (ineqs(k).constraintType == LE) ineqs(k).rhs else -ineqs(k).rhs
    }
    val mA = A.IMat.tabulate(ineqs.size, dim) { (r, c) =>
      if (ineqs(r).constraintType == LE)
        ineqs(r).lhs.getOrElse(c, Ring[A].zero)
      else
        -ineqs(r).lhs.getOrElse(c, Ring[A].zero)
    }
    val vbeq = A.IVec.tabulate(eqs.size)( eqs(_).rhs )
    val mAeq = A.IMat.tabulate(eqs.size, dim) { (r, c) => eqs(r).lhs.getOrElse(c, Ring[A].zero) }
    val Some((dir, objLin)) = objective
    val vobj = A.IVec.tabulate(dim)( k => objLin.coeffs.getOrElse(k, Ring[A].zero) )
    LinearProgram(dir, vobj, HPolytopeM(mA, vb, mAeq, vbeq), Box.unbounded[A](dim))
  }

}
*/