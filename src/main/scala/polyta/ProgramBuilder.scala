package com.faacets
package polyta

import scala.language.implicitConversions
import scala.language.experimental.macros

import scala.{specialized => sp}

import scala.collection.mutable.ArrayBuffer

import spire.algebra._
import spire.math.Rational
import spire.std.map._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

trait LinearProgramBuilder[M, V, @sp(Double) A] { self =>
  implicit def MV: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = MV.V
  implicit def A: Field[A] = MV.scalar

  val varNames = ArrayBuffer.empty[String]

  implicit object Var {
    def apply(name: String): LinExpr = {
      varNames += name
      val index = varNames.size - 1
      LinExpr(Map(index -> A.one), A.zero)
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
    def <=(rhs: LinExpr): Constraint = (this - rhs) <= A.zero
    def >=(rhs: LinExpr): Constraint = (this - rhs) >= A.zero
    def :=(rhs: LinExpr): Constraint = (this - rhs) := A.zero
  }

  object LinExpr {
    def apply(a: A): LinExpr = LinExpr(Map.empty[Int, A], a)
  }

  implicit val LinExprAlgebra: VectorSpace[LinExpr, A] = new VectorSpace[LinExpr, A] {
    def scalar = A
    implicit val MVS: VectorSpace[Map[Int, A], A] = MapVectorSpace[Int, A](A)
    def zero: LinExpr = LinExpr(Map.empty[Int, A], A.zero)
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
  def result: LinearProgram[M, V, A] = {
    val nX = varNames.size
    val (eqs, ineqs) = constraints.partition(_.constraintType == EQ)
    val vb = V.fromFunV(new FunV[A] {
      def len = ineqs.size
      def f(k: Int): A =
        if (ineqs(k).constraintType == LE) ineqs(k).rhs else -ineqs(k).rhs
    })
    val mA = MV.fromFunM(new FunM[A] {
      def nR = ineqs.size
      def nC = nX
      def f(r: Int, c: Int): A =
        if (ineqs(r).constraintType == LE)
          ineqs(r).lhs.getOrElse(c, A.zero)
        else
          -ineqs(r).lhs.getOrElse(c, A.zero)
    })
    val vbeq = V.fromFunV(new FunV[A] {
      def len = eqs.size
      def f(k: Int): A = eqs(k).rhs
    })
    val mAeq = MV.fromFunM(new FunM[A] {
      def nR = eqs.size
      def nC = nX
      def f(r: Int, c: Int): A = eqs(r).lhs.getOrElse(c, A.zero)
    })
    val Some((dir, objLin)) = objective
    val vobj = V.fromFunV(new FunV[A] {
      def len = nX
      def f(k: Int): A = objLin.coeffs.getOrElse(k, A.zero)
    })
    LinearProgram(dir, vobj, HPolyhedron(mA, vb, mAeq, vbeq), Box.unbounded[M, V, A](nX))
  }
}
