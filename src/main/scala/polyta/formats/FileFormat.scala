package com.faacets
package polyta
package formats

import java.io.{Reader, Writer}

import scala.util.parsing.combinator.RegexParsers
import scala.{specialized => sp}

trait FormatRead[Data] {
  trait ParserBase extends RegexParsers {
    def data: Parser[Data]
  }
  val Parser: ParserBase

  def parse(in: Reader): Parser.ParseResult[Data] = Parser.parseAll(Parser.data, in)
  def parse(in: String): Parser.ParseResult[Data] = Parser.parseAll(Parser.data, in)
}

trait FormatWrite[Data] {
  def write(data: Data): String
  def write(data: Data, out: Writer): Unit
}
