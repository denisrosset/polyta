package com.faacets
package polyta
package solvers

import Predef.{any2stringadd => _,_}


import scala.{specialized => sp}
import java.io.{File, PrintWriter, StringReader}

import spire.algebra.Order
import spire.math.Rational
import spire.std.tuples._
import spire.syntax.group._
import spire.util.Opt

import qalg.algebra._
import qalg.algos._

import net.alasc.algebra._
import net.alasc.math.{Grp, Perm}
import net.alasc.std.any._
import net.alasc.syntax.shiftablePermutation._

import formats._
import formats.sympol._

import sys.process._

object Sympol {
  /* Observations:
   * 
   * - when an explicit symmetry group of order 1 is given, Sympol recomputes the symmetry group
   */
  def convertHtoV[M, V, G](hPolytope: HPolytopeCombSym[M, V, Rational, G], recomputeUsingGroup: Opt[Grp[G]] = Opt.empty): VPolytopeFromH[V, Rational, G] = recomputeUsingGroup match {
    case Opt(baseGroup) =>
      import hPolytope.pack
      import hPolytope.orderA
      import baseGroup.{classTag, equality, finiteGroup, representations}
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolytope(hPolytope)
      FormatWrite[IneData[V]].write(ineData, writer)
      writer.close
      val output = ("sympol --cdd -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[ExtData[V]].parse(reader).get
      assert(data.polytope.rays.isEmpty)
      val permutations = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows).map(_._1)
      val generators = permutations.map(perm => baseGroup.find(perm, hPolytope.representation).getOrElse(sys.error("Sympol found a symmetry that cannot be represented by the given group")))
      val symHPolytope = HPolytopeCombSym(hPolytope.mA, hPolytope.vb, hPolytope.mAeq, hPolytope.vbeq, Grp(generators: _*), hPolytope.facetIndexAction)
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      VPolytopeFromH(symHPolytope, indexSets)
    case _ =>
      import hPolytope.pack
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromSymPolytope(hPolytope)
      FormatWrite[IneData[V]].write(ineData, writer)
      writer.close
      val options = if (hPolytope.symGroup.order == 1) "--no-automorphisms -d" else "-a --cdd"
      val output = (s"sympol $options -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[ExtData[V]].parse(reader).get
      assert(data.polytope.rays.isEmpty)
      if (data.polytope.rays.nonEmpty) throw new IllegalArgumentException("Cannot have rays on a bounded polytope.")
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      VPolytopeFromH(hPolytope, indexSets)
  }

  def convertVtoH[M, V, G](vPolytope: VPolytopeCombSym[M, V, Rational, G], recomputeUsingGroup: Opt[Grp[G]] = Opt.empty): HPolytopeFromV[V, Rational, G] = recomputeUsingGroup match {
    case Opt(baseGroup) =>
      assert(vPolytope.rays.isEmpty)
      import vPolytope.pack
      import vPolytope.orderA
      import baseGroup.{classTag, equality, finiteGroup, representations}
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolytope(vPolytope)
      FormatWrite[ExtData[V]].write(extData, writer)
      writer.close
      val output = ("sympol --cdd -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[IneData[V]].parse(reader).get
      val permutations = data.symmetryInfo.get.decodeGenerators(extData.rayRows).map(_._1)
      val generators = permutations.map(perm => baseGroup.find(perm, vPolytope.representation).getOrElse(sys.error("Sympol found a symmetry that cannot be represented by the given group")))
      val symVPolytope = VPolytopeCombSym(vPolytope.allVertexPoints, vPolytope.allRayPoints, Grp(generators: _*), vPolytope.vertexIndexAction, vPolytope.rayIndexAction)
      val indexSets = data.polytope.facets.map(vPolytope.vertexIndexSet(_)).toIndexedSeq
      HPolytopeFromV(symVPolytope, indexSets, data.polytope.equalities.toIndexedSeq)
    case _ =>
      assert(vPolytope.rays.isEmpty)
      import vPolytope.pack
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromSymPolytope(vPolytope)
      FormatWrite[ExtData[V]].write(extData, writer)
      writer.close
      val options = if (vPolytope.symGroup.order == 1) "--no-automorphisms -d" else "-a --cdd"
      val output = (s"sympol $options -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = FormatRead[IneData[V]].parse(reader).get
      val indexSets = data.polytope.facets.map(vPolytope.vertexIndexSet(_)).toIndexedSeq
      HPolytopeFromV(vPolytope, indexSets, data.polytope.equalities.toIndexedSeq)
  }

    def findSymmetriesV[M, V, G](vPolytope: VPolytopeCombSym[M, V, Rational, G], baseGroup: Grp[G]): VPolytopeCombSym[M, V, Rational, G] = {
      assert(vPolytope.rays.isEmpty)
      import vPolytope.{pack, orderA}
      import baseGroup.{classTag, equality, finiteGroup, representations}
      val input = File.createTempFile("sym", ".ine")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolytope(vPolytope)
      FormatWrite[ExtData[V]].write(extData, writer)
      writer.close
      val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val symmetryInfo = FormatRead[SymmetryInfo].parse(reader).get
      val permutations = symmetryInfo.decodeGenerators(extData.rayRows).map(_._1)
      val generators = permutations.map(perm => baseGroup.find(perm, vPolytope.representation).getOrElse(sys.error("Sympol found a symmetry that cannot be represented by the given group")))
      val symVPolytope = VPolytopeCombSym(vPolytope.allVertexPoints, vPolytope.allRayPoints, Grp(generators: _*), vPolytope.vertexIndexAction, vPolytope.rayIndexAction)
      symVPolytope
    }

  def findSymmetriesH[M, V,  G](hPolytope: HPolytopeCombSym[M, V, Rational, G], baseGroup: Grp[G]): HPolytopeCombSym[M, V, Rational, G] = {
    import hPolytope.{pack, orderA}
    import baseGroup.{classTag, equality, finiteGroup, representations}
    val input = File.createTempFile("conv", ".ine")
    val writer = new PrintWriter(input)
    val ineData = IneData.fromPolytope(hPolytope)
    FormatWrite[IneData[V]].write(ineData, writer)
    writer.close
    val output = ("sympol -a -i " + input.getAbsolutePath).!!
    val reader = new StringReader(output)
    val symmetryInfo = FormatRead[SymmetryInfo].parse(reader).get
    val permutations = symmetryInfo.decodeGenerators(ineData.equalityRows).map(_._1)
    val generators = permutations.map(perm => baseGroup.find(perm, hPolytope.representation).getOrElse(sys.error("Sympol found a symmetry that cannot be represented by the given group")))
    val symHPolytope = HPolytopeCombSym(hPolytope.mA, hPolytope.vb, hPolytope.mAeq, hPolytope.vbeq, Grp(generators: _*), hPolytope.facetIndexAction)
    symHPolytope
  }
}
