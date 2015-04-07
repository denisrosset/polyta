package com.faacets
package polyta
package formats

import scala.util.parsing.combinator._

trait ParserUtils extends Parsers {
  def reportException[T](value: => T): Parser[T] = util.Try(value) match {
    case util.Success(t) => success(t)
    case util.Failure(ex) => failure(ex.toString)
  }

  def optionMerge[T](aOption: Option[T], bOption: Option[T], merge: (T, T) => Parser[T]): Parser[Option[T]] =
    aOption match {
      case Some(a) => bOption match {
        case Some(b) => merge(a, b).map(Some(_))
        case None => success(Some(a))
      }
      case None => success(bOption)
    }

  def oneOptionOutOf[T](aOption: Option[T], bOption: Option[T], failureMessage: String = "An element is defined twice"): Parser[Option[T]] =
    optionMerge[T](aOption, bOption, (a, b) => failure(failureMessage))
}
