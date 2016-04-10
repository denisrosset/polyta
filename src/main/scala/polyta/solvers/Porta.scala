package com.faacets
package polyta
package solvers

import java.io.{File, PrintWriter, FileReader}

import spire.math.Rational

import formats._
import formats.porta._

import sys.process._

import scalin.Vec

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

  def toHPolytope(vPolytope: VPolytopeM[Rational])(implicit O: PortaOptions): HPolytopeM[Rational] = {
    val input = new File("test.poi")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[POIData]].write(POIData(vPolytope), writer)
    writer.close
    val cmdline = "traf " + O.optionString + " " + input.getAbsolutePath
    val status = cmdline.!!
    val output = new File("test.poi.ieq")
    val reader = new FileReader(output)
    val ieq = implicitly[FormatRead[IEQData]].parse(reader).get
    ieq.polytope
  }

  def toVPolytope(hPolytope: HPolytopeM[Rational], validPoint: Vec[Rational])(implicit O: PortaOptions): VPolytopeM[Rational] = {
    val input = new File("test.ieq")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IEQData]].write(IEQData(hPolytope, validPoint = Some(validPoint)), writer)
    writer.close
    val status = ("traf " + O.optionString + " " + input.getAbsolutePath).!!
    val output = new File("test.ieq.poi")
    val reader = new FileReader(output)
    val poi = implicitly[FormatRead[POIData]].parse(reader).get
    poi.polytope
  }

}
