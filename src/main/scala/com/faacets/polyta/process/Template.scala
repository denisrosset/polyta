package com.faacets.polyta.process

import java.io.{File, PrintWriter}

import scala.io.Source

import com.faacets.polyta.formats.{FormatRead, FormatWrite}
import sys.process._

import fastparse.core.Parsed

trait Computation[Input, Output] {

  def inputFormat: FormatWrite[Input]

  def outputFormat: FormatRead[Output]

  /** Type representing how the input is communicated to the process, either
    * by standard input (`InputFile = None`) or by a file (`InputFile = Some[File]`).
    */
  type InputFile <: Option[File]

  /** Returns the name of a new temporary input file if `InputFile = Some[File]`,
    * or returns `None` if `InputFile = None`. */
  def newInputFile(): InputFile

  /** Returns the name of a new temporary output file if `OutputFile = Some[File]`,
    * the name of which could depend on `inputFile`; if `OutputFile = None`, then
    * `None` is returned.
    */
  def newOutputFile(inputFile: InputFile): OutputFile

  /** Type representing how the output is provided by the process, either
    * by standard output (`OutputFile = None`) or by a file (`InputFile = Some[File]`).
    */
  type OutputFile <: Option[File]

  /** Returns the command-line used to call the process, with the given input and output
    * filenames.
    */
  def commandLine(inputFile: InputFile, outputFile: OutputFile): String

}

object Runner {

  def throwing[Input, Output](computation: Computation[Input, Output], input: Input): (Output, String) = {
    val inputFile = computation.newInputFile()
    val outputFile = computation.newOutputFile(inputFile)
    inputFile match {
      case Some(inf) => outputFile match {
        case Some(outf) =>
          val log = new StringBuilder
          val writer = new PrintWriter(inf)
          computation.inputFormat.write(input, writer)
          writer.close()
          val stdout = computation.commandLine(inputFile, outputFile).!!
          log ++= stdout
          val outputContent = Source.fromFile(outf, "UTF-8").mkString
          computation.outputFormat.data.parse(outputContent) match {
            case Parsed.Success(output, _) => (output, log.toString)
            case failure@Parsed.Failure(_, _, _) => throw new Exception(log.toString + failure.toString)
          }
        case _: None.type => ???
      }
      case _: None.type => ???
    }
  }

}