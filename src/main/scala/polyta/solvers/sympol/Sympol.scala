package com.faacets
package polyta
package solvers
package sympol

import java.io.{File, PrintWriter, StringReader}

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

object Sympol {
  implicit def symmetryFinderH[V](implicit alg: AlgVF[V, Rational]): SymmetryFinder[HPolytope[V, Rational], SymHPolytope[Perm, V, Rational]] = new SymmetryFinder[HPolytope[V, Rational], SymHPolytope[Perm, V, Rational]] {
    def symmetric(polytope: HPolytope[V, Rational]): SymHPolytope[Perm, V, Rational] = {
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

  implicit def symmetryFinderV[V](implicit alg: AlgVF[V, Rational]): SymmetryFinder[VPolytope[V, Rational], SymVPolytope[Perm, V, Rational]] = new SymmetryFinder[VPolytope[V, Rational], SymVPolytope[Perm, V, Rational]] {
    def symmetric(polytope: VPolytope[V, Rational]): SymVPolytope[Perm, V, Rational] = {
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

  implicit def vReducedDual[V, M](implicit alg: AlgMVF[M, V, Rational]): ReducedDual[VPolytope[V, Rational], VReducedDual[Perm, V, Rational]] = new ReducedDual[VPolytope[V, Rational], VReducedDual[Perm, V, Rational]] {
    def reducedDual(vPolytope: VPolytope[V, Rational]): VReducedDual[Perm, V, Rational] = {
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

  implicit def hReducedDual[V, M](implicit alg: AlgMVF[M, V, Rational]): ReducedDual[HPolytope[V, Rational], HReducedDual[Perm, V, Rational]] = new ReducedDual[HPolytope[V, Rational], HReducedDual[Perm, V, Rational]] {
    def reducedDual(hPolytope: HPolytope[V, Rational]): HReducedDual[Perm, V, Rational] = {
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

  implicit def hDual[V](implicit algVF: AlgVF[V, Rational]): Dual[HPolyhedron[V, Rational], VPolyhedron[V, Rational]] = new Dual[HPolyhedron[V, Rational], VPolyhedron[V, Rational]] {
    def dual(hPolyhedron: HPolyhedron[V, Rational]): VPolyhedron[V, Rational] = {
      val input = File.createTempFile("conv", ".ine")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[IneData[V]]].write(IneData.fromPolyhedron(hPolyhedron), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      implicitly[FormatRead[ExtData[V]]].parse(reader).get.polyhedron
    }
  }

  implicit def vDual[V](implicit algVF: AlgVF[V, Rational]): Dual[VPolyhedron[V, Rational], HPolyhedron[V, Rational]] = new Dual[VPolyhedron[V, Rational], HPolyhedron[V, Rational]] {
    def dual(vPolyhedron: VPolyhedron[V, Rational]): HPolyhedron[V, Rational] = {
      val input = File.createTempFile("conv", ".ext")
      val writer = new PrintWriter(input)
      implicitly[FormatWrite[ExtData[V]]].write(ExtData.fromPolyhedron(vPolyhedron), writer)
      writer.close
      val output = ("sympol --no-automorphisms -d -i " + input.getAbsolutePath).!!
      val reader = new StringReader(output)
      implicitly[FormatRead[IneData[V]]].parse(reader).get.polyhedron
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
