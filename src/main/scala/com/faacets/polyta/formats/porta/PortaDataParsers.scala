package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.immutable.Vec

import scalin.immutable.dense._

import StandardParsers._
import White._
import fastparse.noApi._

object PortaDataParsers {

  val dimSection: P[Int] = P( "DIM" ~ "=" ~ positiveInt )

  def rowVector(d: Int): P[Vec[Rational]] = rational.rep(min=d, max=d)
      .map(seq => Vec.tabulate(d)(seq(_)) )

  val lineNumberForget: P[Unit] = P( "(" ~ nonNegativeInt ~ ")" ).map( x => () )

  val end = P( "END" ~ lineEndings.? ) // content after the END tag is ignored

}
