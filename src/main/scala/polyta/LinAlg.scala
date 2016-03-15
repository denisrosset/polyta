package com.faacets
package polyta

import spire.algebra._

import scalin.algebra._

trait LinAlg[A] {

  implicit def fieldA: Field[A]
  implicit def orderA: Order[A]
  implicit def pivotA: Pivot[A]

  type UMat <: scalin.mutable.Mat[A]
  type UVec <: scalin.mutable.Vec[A]
  def UMat: MatField[A, UMat]
  def UVec: VecField[A, UVec]

  type IMat <: scalin.immutable.Mat[A]
  type IVec <: scalin.immutable.Vec[A]
  def IMat: MatField[A, IMat]
  def IVec: VecField[A, IVec]

  def result(mat: UMat): IMat
  def result(vec: UVec): IVec

}

object LinAlg {

  implicit def dense[A:Order:Field:Pivot]: LinAlg[A] = new LinAlg[A] {

    def fieldA = implicitly[Field[A]]
    def orderA = implicitly[Order[A]]
    def pivotA = implicitly[Pivot[A]]

    type UMat = scalin.mutable.DenseMat[A]
    type UVec = scalin.mutable.DenseVec[A]
    val UMat = scalin.mutable.dense.matField[A]
    val UVec = scalin.mutable.dense.vecField[A]

    type IMat = scalin.immutable.DenseMat[A]
    type IVec = scalin.immutable.DenseVec[A]
    val IMat = scalin.immutable.dense.matField[A]
    val IVec = scalin.immutable.dense.vecField[A]

    def result(mat: UMat): IMat = mat.result()
    def result(vec: UVec): IVec = vec.result()

  }

}

object mutable {

  implicit def matField[A](implicit la: LinAlg[A]): MatField[A, la.UMat] = la.UMat
  implicit def vecField[A](implicit la: LinAlg[A]): VecField[A, la.UVec] = la.UVec

}

object immutable {

  implicit def matField[A](implicit la: LinAlg[A]): MatField[A, la.IMat] = la.IMat
  implicit def vecField[A](implicit la: LinAlg[A]): VecField[A, la.IVec] = la.IVec

}
