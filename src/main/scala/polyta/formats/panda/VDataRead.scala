package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.math.Perm

class VDataRead[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends FormatRead[VData[M, V]] {

  type VPoly = VPolyhedronM[M, V, Rational]
 
  object Parsers extends ParsersBase with PandaDataParsers[V] {

    /* A Panda file can be either named or unnamed.
     * 
     * Only files with variable names can have symmetry information.
     * 
     * If neither names nor dimension are provided, the dimension of unnamed
     * variables is guessed from the first vertices/rays section.
     */

    def raysHeading = "CONE_SECTION" | "Rays:"

    def verticesHeading = "Vertices:" | "CONV_SECTION"

    type Header = (VPoly, Option[Seq[String]])

    def firstRaysSection: Parser[Header] =
      (raysHeading ~ lineEndings) ~> rowVector into { first =>
        rep(lineEndings ~> rowVector(first.length)) ^^ { rest =>
          (VPolyhedronM.fromRays(MatBuilder[M, Rational].fromCols(first.length, first +: rest: _*)), None)
        }
      }

    def firstVerticesSection: Parser[Header] =
      (verticesHeading ~ lineEndings) ~> rowVector into { first =>
        rep(lineEndings ~> rowVector(first.length)) ^^ { rest =>
          (VPolyhedronM.fromVertices(MatBuilder[M, Rational].fromCols(first.length, first +: rest: _*)), None)
        }
      }

    def vUnnamedHeader: Parser[Header] = unnamedHeader ^^ { dim =>
      (VPolyhedronM.empty[M, V, Rational](dim), None)
    }

    def vNamedHeader: Parser[Header] = namedHeader ^^ { seq =>
      (VPolyhedronM.empty(seq.size), Some(seq))
    }

    def vHeader: Parser[Header] = vNamedHeader | vUnnamedHeader | firstRaysSection | firstVerticesSection

    def matrix(cols: Int): Parser[M] = repsep(rowVector(cols), lineEndings) ^^ { rows =>
      MatBuilder[M, Rational].fromRows(cols, rows: _*)
    }

    type Section = Either[Maps[M, V], VPoly]

    def raysSection(dim: Int): Parser[Section] =
      ((raysHeading ~ lineEndings) ~> matrix(dim)) ^^ { m =>
        Right(VPolyhedronM.fromRays[M, V, Rational](m.t))
      }

    def verticesSection(dim: Int): Parser[Section] =
      ((verticesHeading ~ lineEndings) ~> matrix(dim)) ^^ { m =>
        Right(VPolyhedronM.fromVertices[M, V, Rational](m.t))
      }

    def section(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] = raysSection(dim) | verticesSection(dim) | mapsSection(namesOption)

    def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps[M, V], VPoly)] = rep(section(dim, namesOption) <~ sectionEnd) ^^ { eithers =>
      val (mapsSeq, vpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val vpoly = VPolyhedronM.union((VPolyhedronM.empty[M, V, Rational](dim) +: vpolys): _*)
      (maps, vpoly)
    }

    def data = phrase((vHeader <~ sectionEnd) into {
      case (vpoly, namesOption) => sections(vpoly.nX, namesOption) ^^ {
        case (maps, newVpoly) => VData(VPolyhedronM.union(vpoly, newVpoly), namesOption, maps)
      }
    })
  }
}
