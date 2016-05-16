package com.faacets
package polyta

sealed trait SolverStatus[+E]

object SolverStatus {

  case object OptimumFound extends SolverStatus[Nothing]

  case object Infeasible extends SolverStatus[Nothing]

  case object Unbounded extends SolverStatus[Nothing]

  case class SolverFailure[+E](e: E) extends SolverStatus[E]

}
