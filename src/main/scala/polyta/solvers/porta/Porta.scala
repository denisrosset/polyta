package com.faacets
package polyta
package solvers
package porta

import java.io.{File, PrintWriter, FileReader}

import spire.math.Rational

import qalg.algebra._

import formats._
import formats.porta._

import sys.process._

object Porta {
  def toHPolyhedron[M, V](vPoly: VPolyhedron[M, V, Rational])(implicit M: MatVecInField[M, V, Rational]): HPolyhedron[M, V, Rational] = {
    import M.V
    val poi = implicitly[Converter[VPolyhedron[M, V, Rational], POIData[M, V]]].convert(vPoly)
    val input = new File("test.poi")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[POIData[M, V]]].write(poi, writer)
    writer.close
    val status = ("traf -o " + input.getAbsolutePath).!!
    val output = new File("test.poi.ieq")
    val reader = new FileReader(output)
    val ieq = implicitly[FormatRead[IEQData[V]]].parse(reader).get
    implicitly[Converter[IEQData[V], HPolyhedron[M, V, Rational]]].convert(ieq)
  }
}
