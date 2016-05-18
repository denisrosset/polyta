package com.faacets
package polyta
package solvers

import Predef.{any2stringadd => _,_}


import java.io.{File, PrintWriter, StringReader}

import spire.math.Rational
import spire.std.tuples._

import net.alasc.finite.Grp
import net.alasc.perms.Perm

import formats._
import formats.sympol._

import sys.process._


object Sympol {
/*
  def findSymmetriesV(vPolytope: VPolytopeM[Rational]): VPolytopeM[Rational] = {
    assert(vPolytope.rays.isEmpty)
    val input = File.createTempFile("sym", ".ine")
    val writer = new PrintWriter(input)
    val extData = ExtData.fromNonSymPolytope(vPolytope)
    FormatWrite[ExtData].write(extData, writer)
    writer.close
    val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    val symmetryInfo = FormatRead[SymmetryInfo].parse(reader).get
    val permutations = symmetryInfo.decodeGenerators(extData.rayRows).map(_._1)
    VPolytopeM(vPolytope.mV, vPolytope.mR, Grp(permutations.map((_, Perm.id: Perm)): _*)) // TODO: remove :Perm when Alasc updated
  }

  def findSymmetriesH(hPolytope: HPolytopeM[Rational]): HPolytopeM[Rational] = {
    val input = File.createTempFile("conv", ".ine")
    val writer = new PrintWriter(input)
    val ineData = IneData.fromNonSymPolytope(hPolytope)
    FormatWrite[IneData].write(ineData, writer)
    writer.close
    val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    val symmetryInfo = FormatRead[SymmetryInfo].parse(reader).get
    val permutations = symmetryInfo.decodeGenerators(ineData.equalityRows).map(_._1)
    HPolytopeM(hPolytope.mA, hPolytope.vb, hPolytope.mAeq, hPolytope.vbeq, Grp(permutations: _*))
  }*/
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
