package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

import spire.algebra._
import spire.util.Opt

import qalg.algebra._

sealed trait SolverStatus
case object OptimumFound extends SolverStatus
case object Infeasible extends SolverStatus
case object Unbounded extends SolverStatus
case object IterationLimitReached extends SolverStatus
case object TimeLimitReached extends SolverStatus
case object CouldNotSolve extends SolverStatus

trait Result[V, @sp(Double) A]

case class OptimumFound[V, @sp(Double) A](optimalValue: A, optimalSolution: V) extends Result[V, A]

case class Unbounded[V, @sp(Double) A]() extends Result[V, A]
case class Infeasible[V, @sp(Double) A]() extends Result[V, A]

/*
object Result {
  def optimumFound[V, @sp(Double) A](value: A, solution: V)(implicit V0: VecInField[V, A]): Result[V, A] = new Result[V, A] {
    def V = V0
    def status = OptimumFound
    def optimalValue = Opt(value)
    def optimalSolution = Opt(solution)
  }
  def noOptimum[V, @sp(Double) A](status0: SolverStatus)(implicit V0: VecInField[V, A]): Result[V, A] = new Result[V, A] {
    def V = V0
    def status = status0
    def optimalValue = Opt.empty[A]
    def optimalSolution = Opt.empty[V]
  }
}
 */
