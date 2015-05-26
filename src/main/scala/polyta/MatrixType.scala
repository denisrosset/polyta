package com.faacets
package polyta

import scala.{specialized => sp}

import spire.math._

import qalg.math._

trait MatrixType[@sp(Long, Double) A, V, M]

object MatrixType {
  implicit val DenseRational: MatrixType[Rational, DenseV[Rational], DenseM[Rational]] = null
}

