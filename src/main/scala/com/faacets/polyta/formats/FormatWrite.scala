package com.faacets.polyta.formats

import java.io.Writer

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