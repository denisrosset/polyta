package com.faacets
package polyta
package formats

import scala.util.parsing.combinator._

trait AgnosticLineEndingParser extends RegexParsers {
  def crlf = "\r\n" | "\n"
}
