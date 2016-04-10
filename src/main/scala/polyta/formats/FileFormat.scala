package com.faacets
package polyta
package formats

import java.io.{Reader, Writer}

import scala.util.parsing.combinator.RegexParsers

trait Converter[From, To] {
  def convert(from: From): To
}

object Converter {
  def apply[From, To](implicit C: Converter[From, To]): Converter[From, To] = C
}

trait FormatRead[Data] {
  trait ParsersBase extends RegexParsers {
    def data: Parser[Data]
  }
  val Parsers: ParsersBase

  def parse(in: Reader): RegexParsers#ParseResult[Data] = Parsers.parse(Parsers.data, in)
  def parse(in: String): RegexParsers#ParseResult[Data] = Parsers.parse(Parsers.data, in)
}

object FormatRead {
  def apply[Data](implicit F: FormatRead[Data]): FormatRead[Data] = F
}

trait FormatWrite[Data] extends Any {
  def write(data: Data): String = {
    val sw = new java.io.StringWriter
    write(data, sw)
    sw.toString
  }
  def write(data: Data, out: Writer): Unit
}

object FormatWrite {
  def apply[Data](implicit F: FormatWrite[Data]): FormatWrite[Data] = F
}
