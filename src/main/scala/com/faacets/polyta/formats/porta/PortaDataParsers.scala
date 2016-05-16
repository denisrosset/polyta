package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.immutable.Vec

import scalin.immutable.dense._

import scalin.syntax.build.tabulate

import fastparse.WhitespaceApi

object Porta {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \t".contains(_)).?)
  }

}

trait PortaDataParsers extends RationalParsers with AgnosticLineEndingParsers {

  import fastparse.noApi._

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    Porta.White.parserApi(p0)(c)

  val dimSection: P[Int] = P( "DIM" ~ "=" ~ positiveInt )

  def rowVector(d: Int): P[Vec[Rational]] = rational.rep(min=d, max=d)
      .map(seq => tabulate(d)(seq(_)) )

  val lineNumberForget: P[Unit] = P( "(" ~ nonNegativeInt ~ ")" ).map( x => () )

  val end = P( "END" ~ lineEndings.? ) // content after the END tag is ignored

}
