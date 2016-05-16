package com.faacets
package polyta
package solvers

import java.io.{File, PrintWriter}

import spire.math.Rational

import formats._
import formats.porta._

import sys.process._

import scalin.immutable.Vec

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

  def toHPolytope(vPolytope: VPolytope[Rational])(implicit O: PortaOptions): HPolytope[Rational] = {
    val input = new File("test.poi")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[POIData]].write(POIData(vPolytope), writer)
    writer.close
    val cmdline = "traf " + O.optionString + " " + input.getAbsolutePath
    val status = cmdline.!!
    val output = scala.io.Source.fromFile(new File("test.poi.ieq"), "UTF-8").mkString
    val ieq: IEQData = implicitly[FormatRead[IEQData]].data.parse(output).get.value
    ieq.polytope
  }

  def toVPolytope(hPolytope: HPolytope[Rational], validPoint: Vec[Rational])(implicit O: PortaOptions): VPolytope[Rational] = {
    val input = new File("test.ieq")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[IEQData]].write(IEQData(hPolytope, validPoint = validPoint), writer)
    writer.close
    val status = ("traf " + O.optionString + " " + input.getAbsolutePath).!!
    val output = scala.io.Source.fromFile(new File("test.ieq.poi"), "UTF-8").mkString
    val poi: POIData = implicitly[FormatRead[POIData]].data.parse(output).get.value
    poi.polytope
  }

}
