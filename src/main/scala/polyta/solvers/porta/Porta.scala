package com.faacets
package polyta
package solvers
package porta

import java.io.{File, PrintWriter, FileReader}

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

import formats._
import formats.porta._

import sys.process._

trait PortaOptions {
  def useMinimalHeuristic: Boolean = true
  def useChernikovRule: Boolean = true
  def useSpecialArithmetic: Boolean = false
  def optionString: String = {
    val chain = 
      ((if (useMinimalHeuristic) "o" else "") +
        (if (useChernikovRule) "" else "c") +
        (if (useSpecialArithmetic) "l" else "")
      )
    if (chain.nonEmpty) "-" + chain else ""
  }
}

object Porta {
  implicit def DefaultOptions = new PortaOptions { }
  def toHPolyhedron[M, V](vPolyhedron: VPolyhedronM[M, V, Rational])(implicit alg: AlgMVF[M, V, Rational], O: PortaOptions): HPolyhedronM[M, V, Rational] = {
    val input = new File("test.poi")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[POIData[M, V]]].write(POIData(vPolyhedron), writer)
    writer.close
    val status = ("traf " + O.optionString + " " + input.getAbsolutePath).!!
    val output = new File("test.poi.ieq")
    val reader = new FileReader(output)
    val ieq = implicitly[FormatRead[IEQData[M, V]]].parse(reader).get
    ieq.polyhedron
  }

  def toVPolyhedron[M, V](hPolyhedron: HPolyhedronM[M, V, Rational], validPoint: V)(implicit alg: AlgMVF[M, V, Rational]): VPolyhedronM[M, V, Rational] = {
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
