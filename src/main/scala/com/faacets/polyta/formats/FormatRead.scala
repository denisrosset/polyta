package com.faacets
package polyta
package formats

import fastparse.WhitespaceApi
import fastparse.noApi._

trait FormatRead[Data] {

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V]

  def data: P[Data]

}

object FormatRead {

  def apply[Data](implicit F: FormatRead[Data]): FormatRead[Data] = F

}
