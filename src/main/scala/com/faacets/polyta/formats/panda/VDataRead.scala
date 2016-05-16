package com.faacets
package polyta
package formats
package panda

import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._
import scalin.syntax.all._
import spire.math.Rational

import fastparse.WhitespaceApi

import fastparse.noApi._

class VDataRead extends FormatRead[VData] with PandaDataParsers {

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    Panda.White.parserApi(p0)(c)

  type VPoly = VPolytope.Aux[Rational, Symmetry.Without.type]
 
    /* A Panda file can be either named or unnamed.
     * 
     * Only files with variable names can have symmetry information.
     * 
     * If neither names nor dimension are provided, the dimension of unnamed
     * variables is guessed from the first vertices/rays section.
     */

    val raysHeading = P( "CONE_SECTION" | "Rays:" )

    val verticesHeading = P( "Vertices:" | "CONV_SECTION" )

    type Header = (VPoly, Option[Seq[String]])

  def vertcatAll(first: Vec[Rational], rest: Vec[Rational]*): Mat[Rational] =
    tabulate(rest.size + 1, first.length) { (r, c) => if (r == 0) first(c) else rest(r - 1)(c) }

  val firstRaysSection: Parser[Header] = P( raysHeading ~/ lineEndings ~ rowVector ).flatMap { first =>
        (lineEndings ~ rowVector(first.length)).rep.map { rest =>
          (VPolytope.fromRays(vertcatAll(first, rest: _*)), None)
        }
      }

  val firstVerticesSection: P[Header] = (verticesHeading ~ lineEndings ~ rowVector).flatMap { first =>
      (lineEndings ~ rowVector(first.length)).rep.map { rest =>
        (VPolytope.fromVertices(vertcatAll(first, rest: _*)), None)
      }
    }

  val vUnnamedHeader: P[Header] = unnamedHeader.map( dim => (VPolytope.empty[Rational](dim), None) )


  val vNamedHeader: P[Header] = namedHeader.map( seq => (VPolytope.empty(seq.size), Some(seq)) )

  val vHeader: P[Header] = P( vNamedHeader | vUnnamedHeader | firstRaysSection | firstVerticesSection )

  def matrix(cols: Int): P[Mat[Rational]] = rowVector(cols).rep(sep = lineEndings).map { rows =>
    if (rows.size == 0) zeros[Rational](0, cols) else vertcatAll(rows.head, rows.tail: _*)
  }

  type Section = Either[Maps, VPoly]

  def raysSection(dim: Int): P[Section] =
    P(raysHeading ~ lineEndings ~ matrix(dim)).map( m => Right(VPolytope.fromRays[Rational](m)) )

  def verticesSection(dim: Int): P[Section] =
    P(verticesHeading ~ lineEndings ~ matrix(dim)).map( m => Right(VPolytope.fromVertices[Rational](m)) )

  def section(dim: Int, namesOption: Option[Seq[String]]): P[Section] =
    P( raysSection(dim) | verticesSection(dim) | mapsSection(namesOption) )

  def sections(dim: Int, namesOption: Option[Seq[String]]): P[(Maps, VPoly)] =
    (section(dim, namesOption) ~ sectionEnd).rep.map { eithers =>
      val (mapsSeq, vpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val vpoly = vpolys.foldLeft(VPolytope.empty[Rational](dim))( _ union _ )
      (maps, vpoly)
    }

  def data = (vHeader ~ sectionEnd).flatMap {
    case (vpoly, namesOption) => sections(vpoly.dim, namesOption).map {
      case (maps, newVpoly) => VData(vpoly union newVpoly, namesOption, maps)
    }
  }

}
