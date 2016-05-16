package com.faacets
package polyta

sealed trait Direction

object Direction {

  case object Min extends Direction
  case object Max extends Direction

}
