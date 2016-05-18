package com.faacets
package polyta
package solvers

import Predef.{any2stringadd => _, _}
import java.io.{File, PrintWriter, StringReader}

import scala.None

import spire.math.Rational
import spire.std.tuples._

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import formats._
import formats.sympol._
import sys.process._

import com.faacets.polyta.Symmetry.{Combinatorial, Without}
import com.faacets.polyta.VPolytope.Aux
import com.faacets.polyta.process.{Computation, Runner}


object Sympol {

  trait Files[Input, Output] extends Computation[Input, Output] {

    type InputFile = Some[File]
    type OutputFile = None.type

    def inputExtension: String
    def newInputFile(): InputFile = Some(File.createTempFile("sympol", inputExtension))

    def newOutputFile(inputFile: Some[File]) = None

  }

  implicit object FindSymmetriesV extends VPolytope.SymmetryConversion[Rational, Symmetry.Without.type, Symmetry.Combinatorial] {

    object Template extends Files[ExtData, SymmetryInfo] {
      def inputExtension = ".ext"
      def inputFormat: FormatWrite[ExtData] = ExtData.formatWrite
      def outputFormat: FormatRead[SymmetryInfo] = SymmetryInfo.formatRead
      def commandLine(inputFile: Some[File], outputFile: None.type): String =
        "sympol --automorphisms-only -i " + inputFile.get.getAbsolutePath
    }

    def apply(vPoly: VPolytope.Aux[Rational, Symmetry.Without.type])(implicit A: LinAlg[Rational]): VPolytope.Aux[Rational, Combinatorial] = {
      import net.alasc.perms.default._
      assert(vPoly.mR.nRows == 0)
      val extData = ExtData.fromNonSymPolytope(vPoly)
      val (symmetryInfo, _) = Runner.throwing(Template, extData)
      val permutations = symmetryInfo.decodeGenerators(extData.rayRows).map(_._1)
      VPolytope(vPoly.mV, vPoly.mR, Symmetry.Combinatorial(Grp(permutations: _*)))
    }

  }

  implicit object FindSymmetriesH extends HPolytope.SymmetryConversion[Rational, Symmetry.Without.type, Symmetry.Combinatorial] {

    object Template extends Files[IneData, SymmetryInfo] {
      def inputExtension = ".ine"
      def inputFormat: FormatWrite[IneData] = IneData.formatWrite
      def outputFormat: FormatRead[SymmetryInfo] = SymmetryInfo.formatRead
      def commandLine(inputFile: Some[File], outputFile: None.type): String =
        "sympol --automorphisms-only -i " + inputFile.get.getAbsolutePath
    }

    def apply(hPoly: HPolytope.Aux[Rational, Symmetry.Without.type])(implicit A: LinAlg[Rational]): HPolytope.Aux[Rational, Combinatorial] = {
      import net.alasc.perms.default._
      val ineData = IneData.fromNonSymPolytope(hPoly)
      val (symmetryInfo, _) = Runner.throwing(Template, ineData)
      val permutations = symmetryInfo.decodeGenerators(ineData.equalityRows).map(_._1)
      HPolytope(hPoly.mA, hPoly.vb, hPoly.mAeq, hPoly.vbeq, Symmetry.Combinatorial(Grp(permutations: _*)))
    }

  }

  /*
  /* Observations:
   * 
   * - when an explicit symmetry group of order 1 is given, Sympol recomputes the symmetry group
   */
  def convertHtoV(hPolytope: HPolytopeM[Rational], recomputeAutomorphisms: Boolean = false): VPolytopeFromH[Rational] =
    if (recomputeAutomorphisms) {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromNonSymPolytope(hPolytope)
      FormatWrite[IneData].write(ineData, writer)
      writer.close
      val output = ("sympol --cdd -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[ExtData].parse(reader).get
      assert(data.polytope.rays.isEmpty)
      val permutations = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows).map(_._1)
      val symHPolytope = HPolytopeM(hPolytope.mA, hPolytope.vb, hPolytope.mAeq, hPolytope.vbeq, Grp(permutations: _*))
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      new VPolytopeFromH(symHPolytope, indexSets)
    } else {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromSymPolytope(hPolytope)
      FormatWrite[IneData].write(ineData, writer)
      writer.close
      val options = if (hPolytope.symGroup.order == 1) "--no-automorphisms -d" else "-a --cdd"
      val output = (s"sympol $options -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[ExtData].parse(reader).get
      assert(data.polytope.rays.isEmpty)
      if (data.polytope.rays.nonEmpty) throw new IllegalArgumentException("Cannot have rays on a bounded polytope.")
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      new VPolytopeFromH(hPolytope, indexSets)
    }

  def convertVtoH[M, V, G](vPolytope: VPolytopeM[Rational], recomputeAutomorphisms: Boolean = false): HPolytopeFromV[Rational] =
    if (recomputeAutomorphisms) {
      require(vPolytope.rays.isEmpty)
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromNonSymPolytope(vPolytope)
      FormatWrite[ExtData].write(extData, writer)
      writer.close
      val output = ("sympol --cdd -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[IneData].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(extData.rayRows)
      val symVPolytope = VPolytopeM(vPolytope.mV, vPolytope.mR, Grp(generators: _*))
      val indexSets = data.polytope.facets.map(vPolytope.vertexIndexSet(_)).toIndexedSeq
      new HPolytopeFromV(symVPolytope, indexSets, data.polytope.equalities.toIndexedSeq)
    } else {
      require(vPolytope.rays.isEmpty)
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromSymPolytope(vPolytope)
      FormatWrite[ExtData].write(extData, writer)
      writer.close
      val options = if (vPolytope.symGroup.order == 1) "--no-automorphisms -d" else "-a --cdd"
      val output = (s"sympol $options -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[IneData].parse(reader).get
      val indexSets = data.polytope.facets.map(vPolytope.vertexIndexSet(_)).toIndexedSeq
      new HPolytopeFromV(vPolytope, indexSets, data.polytope.equalities.toIndexedSeq)
  }

  */

}
