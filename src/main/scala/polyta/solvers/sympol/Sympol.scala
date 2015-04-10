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

//  def computeAutomorphisms(vPolyhedron: VPolyhedron[M, V, Rational)(implicit

  def toHPolyhedron[M, V](vPolyhedron: VPolyhedron[M, V, Rational])(implicit
    M: MatVecInField[M, V, Rational],
    O: SympolOptions): HPolyhedron[M, V, Rational] = {
    import M.V
    val input = new File("test.ext")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[ExtData[M, V]]].write(ExtData.fromPolyhedron(vPolyhedron), writer)
    writer.close
    val output = ("sympol --no-automorphisms -d " + O.optionString + " -i " + input.getAbsolutePath).!!
      println(output)
    val reader = new StringReader(output)
    implicitly[FormatRead[IneData[M, V]]].parse(reader).get.polyhedron
  }

  def toVPolyhedron[M, V](hPolyhedron: HPolyhedron[M, V, Rational])(implicit
    M: MatVecInField[M, V, Rational],
    O: SympolOptions): VPolyhedron[M, V, Rational] = {
    import M.V
    val input = new File("test.ine")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IneData[M, V]]].write(IneData.fromPolyhedron(hPolyhedron), writer)
    writer.close
    val output = ("sympol --no-automorphisms -d " + O.optionString + " -i " + input.getAbsolutePath).!!
      println(output)
    val reader = new StringReader(output)
    implicitly[FormatRead[ExtData[M, V]]].parse(reader).get.polyhedron
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
