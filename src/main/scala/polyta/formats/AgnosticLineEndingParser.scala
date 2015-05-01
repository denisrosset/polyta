package com.faacets
package polyta
package formats

import scala.util.parsing.combinator._

trait AgnosticLineEndingParsers extends RegexParsers {
  def lineEnding = "\r\n" | "\n"
  def lineEndings = rep1(lineEnding)
}
