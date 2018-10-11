package com.faacets

import spire.algebra.Field
import scalin.Pivot
import scalin.algos.LUDecomposition
import scalin.immutable.{Mat, Vec}

package object polyta {

  def solveLinear[A:Field:Pivot](mA: Mat[A], vb: Vec[A]): Vec[A] = {
    val mutableA = scalin.mutable.dense.matEngine[A].fromMat(mA)
    import scalin.mutable.dense._
    val lu = LUDecomposition.inPlaceLU[A](mutableA)
    scalin.immutable.dense.vecEngine[A].fromVec(lu.solve(vb))
  }

}
