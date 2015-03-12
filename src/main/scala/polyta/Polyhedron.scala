package com.faacets
package polyta

import spire.math.Rational
import spire.syntax.vectorSpace._

import qalg.immutable.{QMatrix, QVector}

/** Polyhedron, i.e. possibly unbounded intersection of half-spaces, given by the
  * equations:
  * 
  * - mA * x <= vb
  * - mAeq * x = vbeq
  */
abstract class Polyhedron extends ConvexSet {
  def nX: Int
  def nIneqs: Int
  def mA: QMatrix
  def vb: QVector
  def mAeq: QMatrix
  def vbeq: QVector
}

/** Fully dimensional polyhedron. */
abstract class FullPolyhedron extends ConvexSet {
  def mA: QMatrix
  def vb: QVector
  def nX = mA.cols
  def nIneqs = mA.rows
  def mAeq: QMatrix = QMatrix.fill(0, nX)(Rational.zero)
  def vbeq: QVector = QVector.fill(0)(Rational.zero)
}

/** Cone defined by Aplus * x >= 0. */
abstract class HCone extends FullPolyhedron {
  def mAplus: QMatrix
  def mA: QMatrix = -mAplus
  def vb: QVector = QVector.fill(nIneqs)(Rational.zero)
}
