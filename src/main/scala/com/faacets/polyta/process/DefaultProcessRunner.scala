package com.faacets.polyta.process

import java.io.File
import java.nio.file.Files
import java.util.concurrent.Executors

import cats.implicits._
import cats.effect._
import io.github.vigoo.prox._
import io.github.vigoo.prox.syntax._
import fs2._

import scala.concurrent.ExecutionContext

object DefaultProcessRunner extends ProcessRunner[IO] {

  type TempFile = java.io.File

  def getFile(prefix: String, suffix: String): IO[TempFile] =
    IO { File.createTempFile(prefix, suffix) }

  def createFile(prefix: String, suffix: String, contents: String): IO[File] = for {
    file <- getFile(prefix, suffix)
    _ <- IO { Files.write(file.toPath, contents.getBytes) }
  } yield file

  def getFileVariant(file: TempFile, removeSuffix: String, appendSuffix: String): IO[TempFile] = {
    val fullPath = file.getAbsolutePath
    if (fullPath.endsWith(removeSuffix))
      IO( new File(fullPath.dropRight(removeSuffix.length) ++ appendSuffix) )
    else
      IO.raiseError(new Exception(s"Path $fullPath does not end with suffix $removeSuffix"))
  }

  def readFile(file: TempFile): IO[String] =
    IO { new String(Files.readAllBytes(file.toPath)) }

  def run(command: String, args: Arg*)(standardInput: String): IO[ProcessOutput] = {
    val stringArgs = args.toList.flatMap {
      case Arg.OfString(x) => List(x)
      case Arg.OfTempFile(x) => List(x.getAbsolutePath)
      case Arg.Empty => Nil
    }
    val inStream = Stream(standardInput).through(text.utf8Encode)
    val process = Process(command, stringArgs) < inStream > text.utf8Decode[IO] redirectErrorTo text.utf8Decode[IO]
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    val blockingExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
    for {
      runningProcess <- process.start(blockingExecutionContext)
      pr <- runningProcess.waitForExit()
    } yield ProcessOutput(pr.exitCode, pr.fullOutput, pr.fullError)
  }

}
