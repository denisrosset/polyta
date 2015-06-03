package com.faacets
package polyta
package solvers
package sympol

import java.io.{File, PrintWriter, StringReader}

import spire.algebra.Order
import spire.math.Rational

import qalg.algebra._

import net.alasc.math.Grp
import net.alasc.std.any._

import formats._
import formats.sympol._

import sys.process._

object Sympol {
  implicit def symmetryFinderH[V](implicit V: VecInField[V, Rational]): SymmetryFinder[HPolyhedron[V, Rational], SymHPolyhedron[V, Rational]] = new SymmetryFinder[HPolyhedron[V, Rational], SymHPolyhedron[V, Rational]] {
    def symmetric(polyhedron: HPolyhedron[V, Rational]): SymHPolyhedron[V, Rational] = {
      val input = File.createTempFile("sym", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolyhedron(polyhedron)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val symInfo = implicitly[FormatRead[SymmetryInfo]].parse(reader).get
      val generators = symInfo.decodeGenerators(ineData.equalityRows).map(_._1)
      SymHPolyhedron[V, Rational](polyhedron.inequalities, polyhedron.equalities, Grp(generators: _*))
    }
  }

  implicit def symmetryFinderV[V](implicit V: VecInField[V, Rational]): SymmetryFinder[VPolyhedron[V, Rational], SymVPolyhedron[V, Rational]] = new SymmetryFinder[VPolyhedron[V, Rational], SymVPolyhedron[V, Rational]] {
    def symmetric(polyhedron: VPolyhedron[V, Rational]): SymVPolyhedron[V, Rational] = {
      val input = File.createTempFile("sym", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolyhedron(polyhedron)
      implicitly[FormatWrite[ExtData[V]]].write(extData, writer)
      writer.close
      val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val symInfo = implicitly[FormatRead[SymmetryInfo]].parse(reader).get
      val generators = symInfo.decodeGenerators(extData.rayCols)
      SymVPolyhedron[V, Rational](polyhedron.vertices, polyhedron.rays, Grp(generators: _*))
    }
  }
}

object WithSymmetries {
  implicit def vConverter[M, V](implicit
    MT: MatType[Rational, V, M],
    M: MatVecInField[M, V, Rational],
    MM: MatMutable[M, Rational],
    VM: VecMutable[V, Rational]): VConverter[VPolyhedron[V, Rational], SymHPolyhedron[V, Rational]] = new VConverter[VPolyhedron[V, Rational], SymHPolyhedron[V, Rational]] {
    def toH(vPolyhedron: VPolyhedron[V, Rational]): SymHPolyhedron[V, Rational] = {
      import M.V
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolyhedron(vPolyhedron)
      implicitly[FormatWrite[ExtData[V]]].write(extData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[IneData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(extData.rayCols)
      val symVPolyhedron = SymVPolyhedron[V, Rational](vPolyhedron.vertices, vPolyhedron.rays, Grp(generators: _*))
      SymHPolyhedronZS.fromDualDescription[M, V, Rational](data.polyhedron, symVPolyhedron)
    }
  }

  implicit def hConverter[M, V](implicit
    MT: MatType[Rational, V, M],
    M: MatVecInField[M, V, Rational],
    MM: MatMutable[M, Rational],
    VM: VecMutable[V, Rational]): HConverter[HPolyhedron[V, Rational], SymVPolyhedron[V, Rational]] = new HConverter[HPolyhedron[V, Rational], SymVPolyhedron[V, Rational]] {
    def toV(hPolyhedron: HPolyhedron[V, Rational]): SymVPolyhedron[V, Rational] = {
      import M.V
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolyhedron(hPolyhedron)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows)
      val symHPolyhedron = SymHPolyhedron[V, Rational](hPolyhedron.inequalities, hPolyhedron.equalities, Grp.fromGenerators(generators.map(_._1)))
      SymVPolyhedronZS.fromDualDescription[M, V, Rational](data.polyhedron, symHPolyhedron)
    }
  }
}

object WithoutSymmetry {

  implicit def hConverter[V](implicit V: VecInField[V, Rational]): HConverter[HPolyhedron[V, Rational], VPolyhedron[V, Rational]] = new HConverter[HPolyhedron[V, Rational], VPolyhedron[V, Rational]] {
    def toV(hPolyhedron: HPolyhedron[V, Rational]): VPolyhedron[V, Rational] = {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[IneData[V]]].write(IneData.fromPolyhedron(hPolyhedron), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      implicitly[FormatRead[ExtData[V]]].parse(reader).get.polyhedron
    }
  }

  implicit def vConverter[V](implicit V: VecInField[V, Rational]): VConverter[VPolyhedron[V, Rational], HPolyhedron[V, Rational]] = new VConverter[VPolyhedron[V, Rational], HPolyhedron[V, Rational]] {
    def toH(vPolyhedron: VPolyhedron[V, Rational]): HPolyhedron[V, Rational] = {
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[ExtData[V]]].write(ExtData.fromPolyhedron(vPolyhedron), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      implicitly[FormatRead[IneData[V]]].parse(reader).get.polyhedron
    }
  }
}


/*
sealed trait SympolMethod {
  def optionString: String
}

/** Direct dual description */
case object Direct extends SympolMethod {
  def optionString = "-d"
}

/** One level of adjacency */
case object Adjacency extends SympolMethod {
  def optionString = "-a"
}

case class IdmAdm(levelIdm: Int, levelAdm: Int) extends SympolMethod {
  def optionString = s"--idm-adm-level $levelIdm $levelAdm"
}

case class AdmIdm(levelAdm: Int, levelIdm: Int) extends SympolMethod {
  def optionString = s"--adm-idm-level $levelAdm $levelIdm"
}

trait SympolOptions {
/*  def useBliss: Boolean = true*/
  def useCdd: Boolean = false
/*  def method: SympolMethod = Direct*/
  def verbose: Boolean = false
  def optionString: String = {
    val useCddOption = if (useCdd) Some("--cdd") else None
    val verboseOption = if (verbose) Some("-v") else None
    (/*Seq(method.optionString) ++ */ useCddOption ++ verboseOption).mkString(" ")
  }
}

*/
