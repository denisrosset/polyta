package com.faacets
package polyta

import qalg.immutable.QVector

sealed trait Objective

case class Maximize(f: QVector) extends Objective {
  def nX: Int = f.length
}

case class Minimize(f: QVector) extends Objective {
  def nX: Int = f.length
}

case class Problem[CS <: ConvexSet](objective: Objective, set: CS)
