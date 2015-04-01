package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

class VDataRead[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatRead[VData[M, V]] {
  implicit def V: VecInField[V, Rational] = M.V

  object Parser extends ParserBase with PandaDataParser[V] with OptionParser  {

    def vNamesSection: Parser[VData[M, V]] = namesSection.map(seq => VData[M, V](names = Some(seq)))
    def vMapsSection: Parser[VData[M, V]] = mapsSection.map(seq => VData[M, V](maps = seq))

    def matrix: Parser[M] = repsep(rowVector, lineEndings) into { rows =>
      util.Try(M.vertcat(rows.map(_.rowMat[M]): _*)) match {
        case util.Success(mat) => success(mat)
        case util.Failure(ex) => failure(ex.toString)
      }
    }

    def raysHeading = "CONE_SECTION" | "Rays"
    def raysSection: Parser[VData[M, V]] =
      ((raysHeading ~ lineEndings) ~> matrix).map { m =>
        VData[M, V](polyhedron = Some(VPolyhedron.fromRays[M, V, Rational](m)))
      }

    def verticesHeading = "Vertices" | "CONV_SECTION"
    def verticesSection: Parser[VData[M, V]] =
      ((verticesHeading ~ lineEndings) ~> matrix).map { m =>
        VData[M, V](polyhedron = Some(VPolyhedron.fromVertices[M, V, Rational](m)))
      }

    def vDimSection: Parser[VData[M, V]] = dimSection.map(d => VData[M, V](dim = Some(d)))

    def section: Parser[VData[M, V]] = vDimSection | vNamesSection | vMapsSection

    def data = phrase(sections)
  }
}
