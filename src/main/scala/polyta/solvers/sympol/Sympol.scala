package com.faacets
package polyta
package solvers
package sympol

import java.io.{File, PrintWriter, StringReader}

import spire.math.Rational

import qalg.algebra._

import net.alasc.math.Grp
import net.alasc.std.any._

import formats._
import formats.sympol._

import sys.process._

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
}*/

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

object Sympol {
  implicit def DefaultOptions = new SympolOptions { }

/*  def toHPolyhedronSym[M, V](vPolyhedron: VPolyhedron[M, V, Rational](implicit
    M: MatVecInField[M, V, Rational],
    O: SympolOptions): HPolyhedron[M, V, Rational] = {
    ???
  }*/

  def toHPolyhedron[V](vPolyhedron: VPolyhedron[V, Rational])(implicit
    V: VecInField[V, Rational],
    O: SympolOptions): HPolyhedron[V, Rational] = {
    val input = new File("test.ext")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[ExtData[V]]].write(ExtData.fromPolyhedron(vPolyhedron), writer)
    writer.close
    val output = ("sympol --no-automorphisms -d " + O.optionString + " -i " + input.getAbsolutePath).!!
      println(output)
    val reader = new StringReader(output)
    implicitly[FormatRead[IneData[V]]].parse(reader).get.polyhedron
  }

  def toVPolyhedron[V](hPolyhedron: HPolyhedron[V, Rational])(implicit
    V: VecInField[V, Rational],
    O: SympolOptions): VPolyhedron[V, Rational] = {
    val input = new File("test.ine")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IneData[V]]].write(IneData.fromPolyhedron(hPolyhedron), writer)
    writer.close
    val output = ("sympol --no-automorphisms -d " + O.optionString + " -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    implicitly[FormatRead[ExtData[V]]].parse(reader).get.polyhedron
  }

  def findSymmetries[V](polyhedron: VPolyhedron[V, Rational])(implicit
    V: VecInField[V, Rational]): SymVPolyhedron[V, Rational] = {
    val input = new File("test.ext")
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

  def findSymmetries[V](polyhedron: HPolyhedron[V, Rational])(implicit
    V: VecInField[V, Rational]): SymHPolyhedron[V, Rational] = {
    val input = new File("test.ine")
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
