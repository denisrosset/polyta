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
  def toHPolyhedron[M, V](vPolyhedron: VPolyhedron[M, V, Rational])(implicit M: MatVecInField[M, V, Rational]): HPolyhedron[M, V, Rational] = {
    import M.V
    val input = new File("test.poi")
    val writer = new PrintWriter(input)
    implicitly[FormatWrite[POIData[M, V]]].write(POIData(vPolyhedron), writer)
    writer.close
    val status = ("traf -o " + input.getAbsolutePath).!!
    val output = new File("test.poi.ieq")
    val reader = new FileReader(output)
    val ieq = implicitly[FormatRead[IEQData[M, V]]].parse(reader).get
    ieq.polyhedron
  }

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
