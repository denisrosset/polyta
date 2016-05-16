package com.faacets
package polyta
package formats

import fastparse.{ParserApi, WhitespaceApi}
import fastparse.noApi._

trait AgnosticLineEndingParsers {

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): ParserApi[V]

  val lineEnding: P[Unit] = P("\r".? ~ "\n")

  val lineEndings: P[Unit] = lineEnding.rep(min=1)
  
}
