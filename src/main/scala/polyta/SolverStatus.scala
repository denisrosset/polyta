package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

import spire.algebra._
import spire.util.Opt

import qalg.algebra._

sealed trait SolverStatus[+E]
case object OptimumFound extends SolverStatus[Nothing]
case object Infeasible extends SolverStatus[Nothing]
case object Unbounded extends SolverStatus[Nothing]
case class SolverFailure[+E](e: E) extends SolverStatus[E]
