package com.faacets.polyta

import fastparse.WhitespaceApi

package object formats {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhile(" \t".contains(_)).?)
  }

}
