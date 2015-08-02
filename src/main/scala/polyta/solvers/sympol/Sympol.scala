package com.faacets
package polyta
package solvers
package sympol

import java.io.{File, PrintWriter, StringReader}

import spire.NoImplicit
import spire.algebra.Order
import spire.math.Rational

import qalg.algebra._
import qalg.algos._

import net.alasc.algebra._
import net.alasc.math.{Grp, Perm}
import net.alasc.std.any._

import formats._
import formats.sympol._

import sys.process._
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

  implicit def vSymReducedDual[G, V, M](implicit alg: AlgMVF[M, V, Rational]): ReducedDual[SymVPolytope[G, V, Rational], VReducedDual[G, V, Rational]] = new ReducedDual[SymVPolytope[G, V, Rational], VReducedDual[G, V, Rational]] {
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
  }

  implicit def hSymReducedDual[G, V, M](implicit alg: AlgMVF[M, V, Rational]): ReducedDual[SymHPolytope[G, V, Rational], HReducedDual[G, V, Rational]] = new ReducedDual[SymHPolytope[G, V, Rational], HReducedDual[G, V, Rational]] {
    def reducedDual(hPolytope: SymHPolytope[G, V, Rational]): HReducedDual[G, V, Rational] = {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      val ineData = IneData.fromSymPolytope(hPolytope)
      implicitly[FormatWrite[IneData[V]]].write(ineData, writer)
      writer.close
      val output = ("sympol -a -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      val data = implicitly[FormatRead[ExtData[V]]].parse(reader).get
      val generators = data.symmetryInfo.get.decodeGenerators(ineData.equalityRows)
      if (data.polyhedron.rays.nonEmpty) throw new IllegalArgumentException("Cannot have rays on a bounded polytope.")
      val vPolytope = VPolytope[V, Rational](data.polyhedron.vertices)
      HReducedDual(hPolytope, vPolytope)
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
}

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
