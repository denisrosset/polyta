package com.faacets.polyta.process

case class ProcessOutput(exitCode: Int, standardOutput: String, standardError: String)

trait ProcessRunner[F[_]] {

  type TempFile

  sealed trait Arg

  object Arg {
    case object Empty extends Arg
    case class OfString(string: String) extends Arg
    case class OfTempFile(tempFile: TempFile) extends Arg
    implicit def fromString(string: String): Arg = OfString(string)
    implicit def fromTempFile(tempFile: TempFile): Arg = OfTempFile(tempFile)
  }

  /** Creates a temporary filename intended to be used as an output filename passed to the process. */
  def getFile(prefix: String, suffix: String): F[TempFile]

  /** Creates a temporary filename and fills it with the given contents. */
  def createFile(prefix: String, suffix: String, contents: String): F[TempFile]

  /** Returns a modification of a temporary filename. */
  def getFileVariant(file: TempFile, removeSuffix: String, appendSuffix: String): F[TempFile]

  /** Reads the temporary file contents. */
  def readFile(file: TempFile): F[String]

  def run(command: String, args: Arg*)(standardInput: String): F[ProcessOutput]
}
