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
  def convert[M, V, G](hPolytope: HPolytopeCombSym[M, V, Rational, G], recomputeUsingGroup: Opt[Grp[G]] = Opt.empty): VPolytopeFromH[V, Rational, G] = recomputeUsingGroup match {
    case Opt(symGroup) =>
      import hPolytope.pack
      import hPolytope.orderA
      import symGroup.{classTag, equality, finiteGroup, representations}
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolytope(hPolytope)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get
      val permutations = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows).map(_._1)
      println(permutations)
      val generators = permutations.map(perm => symGroup.find(perm, symGroup.representation).getOrElse(sys.error("Sympol found a symmetry that cannot be represented by the given group")))
      val symHPolytope = HPolytopeCombSym(hPolytope.mA, hPolytope.vb, hPolytope.mAeq, hPolytope.vbeq, Grp(generators: _*), hPolytope.facetIndexAction)
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      VPolytopeFromH(symHPolytope, indexSets)
    case _ =>
      import hPolytope.pack
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromSymPolytope(hPolytope)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val options = if (hPolytope.symGroup.order == 1) "--no-automorphisms -d" else "-a"
      val output = (s"sympol $options -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get
      if (data.polytope.rays.nonEmpty) throw new IllegalArgumentException("Cannot have rays on a bounded polytope.")
      val indexSets = data.polytope.vertices.map(hPolytope.facetIndexSet(_)).toIndexedSeq
      VPolytopeFromH(hPolytope, indexSets)
  }
}


/*  implicit def vSymReducedDual[G, V, M](implicit alg: AlgMVF[M, V, Rational]): ReducedDual[SymVPolytope[G, V, Rational], VReducedDual[G, V, Rational]] = new ReducedDual[SymVPolytope[G, V, Rational], VReducedDual[G, V, Rational]] {
    def reducedDual(vPolytope: SymVPolytope[G, V, Rational]): VReducedDual[G, V, Rational] = {
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromSymPolytope(vPolytope)
      implicitly[FormatWrite[ExtData[V]]].write(extData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[IneData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(extData.rayCols).map(_._1)
      val hPolytope = HPolytope(data.polyhedron.facets, data.polyhedron.equalities)
      VReducedDual(vPolytope, hPolytope)
    }
  }*/

/*
trait SympolLowerPriority {
  implicit def vReducedDual[P <: VPolytope[V, Rational], V, M](implicit alg: AlgMVF[M, V, Rational], ev: NoImplicit[P <:< SymVPolytope[_, V, Rational]]): ReducedDual[P, VReducedDual[Perm, V, Rational]] = new ReducedDual[P, VReducedDual[Perm, V, Rational]] {
    def reducedDual(vPolytope: P): VReducedDual[Perm, V, Rational] = {
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolyhedron(vPolytope)
      implicitly[FormatWrite[ExtData[V]]].write(extData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[IneData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(extData.rayCols).map(_._1)
      val symVPolytope = SymVPolytope[Perm, V, Rational](vPolytope.vertices, Grp(generators: _*), PermutationRepresentations[Perm].forSize(vPolytope.vertices.size))
      val hPolytope = HPolytope(data.polyhedron.facets, data.polyhedron.equalities)
      VReducedDual(symVPolytope, hPolytope)
    }
  }

  implicit def hReducedDual[P <: HPolytope[V, Rational], V, M](implicit alg: AlgMVF[M, V, Rational], ev: NoImplicit[P <:< SymHPolytope[_, V, Rational]]): ReducedDual[P, HReducedDual[Perm, V, Rational]] = new ReducedDual[P, HReducedDual[Perm, V, Rational]] {
    def reducedDual(hPolytope: P): HReducedDual[Perm, V, Rational] = {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolyhedron(hPolytope)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows)
      if (data.polyhedron.rays.nonEmpty) throw new IllegalArgumentException("Cannot have rays on a bounded polytope.")
      val symHPolytope = SymHPolytope[Perm, V, Rational](hPolytope.facets, hPolytope.equalities, Grp.fromGenerators(generators.map(_._1)), PermutationRepresentations[Perm].forSize(hPolytope.facets.size))
      val vPolytope = VPolytope[V, Rational](data.polyhedron.vertices)
      HReducedDual(symHPolytope, vPolytope)
    }
  }
}*/

/*
trait SympolImplicits {
  implicit def symmetricH[P <: HPolytope[V, Rational], V](implicit alg: AlgVF[V, Rational], ev: NoImplicit[P <:< SymHPolytope[_, V, Rational]]): Symmetric[P, SymHPolytope[Perm, V, Rational]] = new Symmetric[P, SymHPolytope[Perm, V, Rational]] {
    def symmetric(polytope: P): SymHPolytope[Perm, V, Rational] = {
      val input = File.createTempFile("sym", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromPolyhedron(polytope)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val symInfo = implicitly[FormatRead[SymmetryInfo]].parse(reader).get
      val generators = symInfo.decodeGenerators(ineData.equalityRows).map(_._1)
      SymHPolytope[Perm, V, Rational](polytope.facets, polytope.equalities, Grp(generators: _*), PermutationRepresentations[Perm].forSize(polytope.facets.size))
    }
  }

  implicit def symmetricV[P <: VPolytope[V, Rational], V](implicit alg: AlgVF[V, Rational], ev: NoImplicit[P <:< SymVPolytope[_, V, Rational]]): Symmetric[P, SymVPolytope[Perm, V, Rational]] = new Symmetric[P, SymVPolytope[Perm, V, Rational]] {
    def symmetric(polytope: P): SymVPolytope[Perm, V, Rational] = {
      val input = File.createTempFile("sym", ".ext")
      val writer = new PrintWriter(input)
      val extData = ExtData.fromPolyhedron(polytope)
      implicitly[FormatWrite[ExtData[V]]].write(extData, writer)
      writer.close
      val output = ("sympol --automorphisms-only -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val symInfo = implicitly[FormatRead[SymmetryInfo]].parse(reader).get
      val generators = symInfo.decodeGenerators(extData.rayCols).map(_._1)
      SymVPolytope[Perm, V, Rational](polytope.vertices, Grp(generators: _*), PermutationRepresentations[Perm].forSize(polytope.vertices.size))
    }
  }


  implicit def hDual[V](implicit algVF: AlgVF[V, Rational]): Dual[HPolytope[V, Rational], VPolytope[V, Rational]] = new Dual[HPolytope[V, Rational], VPolytope[V, Rational]] {
    def dual(hPolytope: HPolytope[V, Rational]): VPolytope[V, Rational] = {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[IneData[V]]].write(IneData.fromPolyhedron(hPolytope), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get.polyhedron
      assert(data.rays.isEmpty)
      VPolytope(data.vertices)
      
    }
  }

  implicit def vDual[V](implicit algVF: AlgVF[V, Rational]): Dual[VPolytope[V, Rational], HPolytope[V, Rational]] = new Dual[VPolytope[V, Rational], HPolytope[V, Rational]] {
    def dual(vPolytope: VPolytope[V, Rational]): HPolytope[V, Rational] = {
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[ExtData[V]]].write(ExtData.fromPolyhedron(vPolytope), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[IneData[V]]].parse(reader).get.polyhedron
      HPolytope(data.facets, data.equalities)
    }
  }
}*/

/*
 sealed trait SympolMethod {
 def optionString: String
 }

 /** Direct dual description */
 case object Direct extends SympolMethod {
 def optionString = "-d"
 }

 /** One level of adjacency */
 case object Adjacency extends SympolMethod {
 def optionString = "-a"
 }

 case class IdmAdm(levelIdm: Int, levelAdm: Int) extends SympolMethod {
 def optionString = s"--idm-adm-level $levelIdm $levelAdm"
 }

 case class AdmIdm(levelAdm: Int, levelIdm: Int) extends SympolMethod {
 def optionString = s"--adm-idm-level $levelAdm $levelIdm"
 }

 trait SympolOptions {
 /*  def useBliss: Boolean = true*/
 def useCdd: Boolean = false
 /*  def method: SympolMethod = Direct*/
 def verbose: Boolean = false
 def optionString: String = {
 val useCddOption = if (useCdd) Some("--cdd") else None
 val verboseOption = if (verbose) Some("-v") else None
 (/*Seq(method.optionString) ++ */ useCddOption ++ verboseOption).mkString(" ")
 }
 }

 */
