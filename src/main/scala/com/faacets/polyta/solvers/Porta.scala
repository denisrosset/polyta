package com.faacets
package polyta
package solvers

import java.io.{File, PrintWriter}

import spire.math.Rational

import formats._
import formats.porta._
import sys.process._

import com.faacets.polyta.process.{Computation, Runner}
import scalin.immutable.Vec

object Porta {

  trait Files[Input, Output] extends Computation[Input, Output] {

    type InputFile = Some[File]
    type OutputFile = Some[File]

    def inputExtension: String

    def outputAddedExtension: String

    def newInputFile(): InputFile = Some(File.createTempFile("porta", inputExtension))

    def newOutputFile(inputFile: InputFile): OutputFile =
      Some(new File(inputFile.get.getAbsolutePath + outputAddedExtension))

    def optionString: String

    def commandLine(inputFile: InputFile, outputFile: OutputFile): String = {
      val inputPath = inputFile.get.getAbsolutePath
      val outputPath = outputFile.get.getAbsolutePath
      require(outputPath == inputPath + outputAddedExtension)
      "traf " + optionString + " " + inputPath
    }

  }

  trait Options { self: Files[_, _] =>

    def minimalHeuristic: Boolean
    def chernikovRule: Boolean
    def specialArithmetic: Boolean

    def optionString: String = {
      val chain =
        ((if (minimalHeuristic) "o" else "") +
          (if (chernikovRule) "" else "c") +
          (if (specialArithmetic) "l" else "")
          )
      if (chain.nonEmpty) "-" + chain else ""
    }

  }

  case class HToV(minimalHeuristic: Boolean = true, chernikovRule: Boolean = true, specialArithmetic: Boolean = false)
    extends Files[IEQData, POIData] with Options {

    def inputFormat = IEQData.formatWrite

    def outputFormat = POIData.formatRead

    def inputExtension = ".ieq"

    def outputAddedExtension = ".poi"

  }

  def toHPolytope(vPolytope: VPolytope[Rational]): HPolytope[Rational] = {
    val (ieqData, _) = Runner.throwing(VToH(), POIData(vPolytope))
    ieqData.polytope
  }

  case class VToH(minimalHeuristic: Boolean = true, chernikovRule: Boolean = true, specialArithmetic: Boolean = false)
    extends Files[POIData, IEQData] with Options {

    def inputFormat = POIData.formatWrite

    def outputFormat = IEQData.formatRead

    def inputExtension = ".poi"

    def outputAddedExtension = ".ieq"
  }

  def toVPolytope(hPolytope: HPolytope[Rational], validPoint: Vec[Rational]): VPolytope[Rational] = {
    val (poiData, _) = Runner.throwing(HToV(), IEQData(hPolytope, validPoint = validPoint))
    poiData.polytope
  }

}
