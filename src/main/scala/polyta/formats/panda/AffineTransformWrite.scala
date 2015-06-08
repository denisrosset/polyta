package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

import net.alasc.math._
import net.alasc.syntax.all._

class AffineTransformWrite[M, V](val variableNames: Seq[String])(implicit val alg: AlgMVF[M, V, Rational]) extends FormatWrite[AffineTransform[M, V, Rational]] {

  def writeImage(v: V, constant: Rational, out: Writer): Unit = {
    Format.writeVector(v, variableNames, out, constantOpt = Opt(constant), withSpaces = false)
    if (constant != 0) {
      if (constant > 0)
        out.write("+")
      out.write(constant.toString)
    }
  }

  def write(data: AffineTransform[M, V, Rational], out: Writer): Unit = {
    var space = ""
    cforRange(0 until data.nX) { r =>
      out.write(space)
      writeImage(data.mA(r, ::), data.vb(r), out)
      space = " "
    }
  }
}
