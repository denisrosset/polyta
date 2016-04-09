package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import scalin.{Mat, Vec}
import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}
import scalin.immutable.dense._
import scalin.syntax.all._

import spire.math.Rational

class VDataRead extends FormatRead[VData] {

  type VPoly = VPolytopeM[Rational]
 
  object Parsers extends ParsersBase with PandaDataParsers {

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

    def vertcatAll(first: Vec[Rational], rest: Vec[Rational]*): IMat[Rational] =
      IMat.tabulate(rest.size + 1, first.length) { (r, c) => if (r == 0) first(c) else rest(r - 1)(c) }

    def firstRaysSection: Parser[Header] =
      (raysHeading ~ lineEndings) ~> rowVector into { first =>
        rep(lineEndings ~> rowVector(first.length)) ^^ { rest =>
          (VPolytopeM.fromRays(vertcatAll(first, rest: _*)), None)
        }
      }

    def firstVerticesSection: Parser[Header] =
      (verticesHeading ~ lineEndings) ~> rowVector into { first =>
        rep(lineEndings ~> rowVector(first.length)) ^^ { rest =>
          (VPolytopeM.fromVertices(vertcatAll(first, rest: _*)), None)
        }
      }

    def vUnnamedHeader: Parser[Header] = unnamedHeader ^^ { dim =>
      (VPolytopeM.empty[Rational](dim), None)
    }

    def vNamedHeader: Parser[Header] = namedHeader ^^ { seq =>
      (VPolytopeM.empty(seq.size), Some(seq))
    }

    def vHeader: Parser[Header] = vNamedHeader | vUnnamedHeader | firstRaysSection | firstVerticesSection

    def matrix(cols: Int): Parser[IMat[Rational]] = repsep(rowVector(cols), lineEndings) ^^ { rows =>
      if (rows.size == 0) zeros[Rational](0, cols) else vertcatAll(rows.head, rows.tail: _*)
    }

    type Section = Either[Maps, VPoly]

    def raysSection(dim: Int): Parser[Section] =
      ((raysHeading ~ lineEndings) ~> matrix(dim)) ^^ { m =>
        Right(VPolytopeM.fromRays[Rational](m))
      }

    def verticesSection(dim: Int): Parser[Section] =
      ((verticesHeading ~ lineEndings) ~> matrix(dim)) ^^ { m =>
        Right(VPolytopeM.fromVertices[Rational](m))
      }

    def section(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] = raysSection(dim) | verticesSection(dim) | mapsSection(namesOption)

    def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps, VPoly)] = rep(section(dim, namesOption) <~ sectionEnd) ^^ { eithers =>
      val (mapsSeq, vpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val vpoly = vpolys.foldLeft(VPolytopeM.empty[Rational](dim)) { VPolytopeM.WithoutSym.union(_, _) }
      (maps, vpoly)
    }

    def data = phrase((vHeader <~ sectionEnd) into {
      case (vpoly, namesOption) => sections(vpoly.dim, namesOption) ^^ {
        case (maps, newVpoly) => VData(VPolytopeM.WithoutSym.union(vpoly, newVpoly), namesOption, maps)
      }
    })
  }
}
