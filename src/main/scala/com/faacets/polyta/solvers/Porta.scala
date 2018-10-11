package com.faacets
package polyta
package solvers

import cats.effect.IO
import spire.math.Rational
import formats.porta._
import fastparse.noApi.Parsed
import scalin.immutable.Vec

import process.{DefaultProcessRunner => runner}

object Porta {

  case class Options(minimalHeuristic: Boolean, chernikovRule: Boolean, specialArithmetic: Boolean) {
    def arg[F[_]](r: process.ProcessRunner[F]): r.Arg = {
      val chain =
        ((if (minimalHeuristic) "o" else "") +
          (if (chernikovRule) "" else "c") +
          (if (specialArithmetic) "l" else "")
          )
      if (chain.nonEmpty) r.Arg.OfString("-" + chain) else r.Arg.Empty
    }
  }

  object Options {
    val fastUnsafe = Options(true, true, false)
  }

  type EitherPF[X] = Either[Parsed.Failure, X]

  def toHPolytope(vPolytope: VPolytope[Rational], options: Options = Options.fastUnsafe): HPolytope[Rational] = {
    val inputData = POIData(vPolytope)
    val io = for {
      inputFile <- runner.createFile("porta", ".poi", inputData.fileContents)
      outputFile <- runner.getFileVariant(inputFile, "", ".ieq")
      processOutput <- runner.run("traf", options.arg(runner), inputFile)("")
      result <- runner.readFile(outputFile)
      parsed <- IO { IEQData.parser.parse(result).get }
    } yield parsed.value.polytope
    io.unsafeRunSync()
  }

  def toVPolytope(hPolytope: HPolytope[Rational], validPoint: Vec[Rational], options: Options = Options.fastUnsafe): VPolytope[Rational] = {
    val inputData = IEQData(hPolytope, validPoint)
    val io = for {
      inputFile <- runner.createFile("porta", ".ieq", inputData.fileContents)
      outputFile <- runner.getFileVariant(inputFile, "", ".poi")
      processOutput <- runner.run("traf", options.arg(runner), inputFile)("")
      result <- runner.readFile(outputFile)
      parsed <- IO { POIData.parser.parse(result).get }
    } yield parsed.value.polytope
    io.unsafeRunSync()
  }

}
