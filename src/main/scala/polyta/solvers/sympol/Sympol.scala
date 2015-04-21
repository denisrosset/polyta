package com.faacets
package polyta
package solvers
package sympol

import java.io.{File, PrintWriter, StringReader}

import spire.math.Rational

import qalg.algebra._

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
    V: VecInField[V, Rational]): SymmetryInfo = {
    val input = new File("test.ext")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[ExtData[V]]].write(ExtData.fromPolyhedron(polyhedron), writer)
    writer.close
    val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    implicitly[FormatRead[SymmetryInfo]].parse(reader).get
  }

  def findSymmetries[V](polyhedron: HPolyhedron[V, Rational])(implicit
    V: VecInField[V, Rational]): SymmetryInfo = {
    val input = new File("test.ine")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IneData[V]]].write(IneData.fromPolyhedron(polyhedron), writer)
    writer.close
    val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    implicitly[FormatRead[SymmetryInfo]].parse(reader).get
  }
}

/*
object Porta {
  implicit def DefaultOptions = new PortaOptions { }

  def toVPolyhedron[M, V](hPolyhedron: HPolyhedron[M, V, Rational], validPoint: V)(implicit M: MatVecInField[M, V, Rational]): VPolyhedron[M, V, Rational] = {
    import M.V
    val input = new File("test.ieq")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IEQData[M, V]]].write(IEQData(hPolyhedron, validPoint = Some(validPoint)), writer)
    writer.close
    val status = ("traf -o " + input.getAbsolutePath).!!
    val output = new File("test.ieq.poi")
    val reader = new FileReader(output)
    val poi = implicitly[FormatRead[POIData[M, V]]].parse(reader).get
    poi.polyhedron
  }
}
 */
