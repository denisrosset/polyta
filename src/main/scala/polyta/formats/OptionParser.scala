package com.faacets
package polyta
package formats

import scala.util.parsing.combinator._

trait OptionParser extends Parsers {
  def oneOptionOutOf[T](aOption: Option[T], bOption: Option[T]): Parser[Option[T]] =
    aOption match {
      case Some(a) => bOption match {
        case Some(b) => failure("A section is defined twice.")
        case None => success(Some(a))
      }
      case None => success(bOption)
    }
}

