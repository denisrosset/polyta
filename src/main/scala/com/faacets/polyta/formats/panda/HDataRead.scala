package com.faacets
package polyta
package formats
package panda

import spire.math.Rational
import spire.std.map._
import spire.syntax.field._

import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._
import scalin.syntax.all._
import ComparisonOp._
import fastparse.WhitespaceApi

import fastparse.noApi._

class HDataRead extends FormatRead[HData] with PandaDataParsers with NamedExprParsers {

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    Panda.White.parserApi(p0)(c)

  type VCons = LinearConstraint[Rational]
  type HPoly = HPolytope.Aux[Rational, Symmetry.Without.type]

  /* A Panda file can be either named or unnamed.
   *
   * Only files with variable names can have symmetry information.
   *
   * If neither names nor dimension are provided, the dimension of unnamed
   * variables is guessed from the first Equations/Inequalities section.
   */

  def vecEquality: P[VCons] = rational.rep.map( seq => LinearEquality(vec(seq.init:_*), -seq.last) )

  def vecEquality(dim: Int): P[VCons] = (rational.rep(min=dim, max=dim) ~ rational).map {
    case (lhs, minusRhs) => LinearEquality(vec(lhs: _*), -minusRhs)
  }

  def vecInequality: P[VCons] = rational.rep.map( seq => LinearInequality(vec(seq.init:_*), LE, -seq.last) )

  def vecInequality(dim: Int): P[VCons] = (rational.rep(min=dim, max=dim) ~ rational).map {
    case (lhs, minusRhs) => LinearInequality(vec(lhs: _*), LE, -minusRhs)
  }

  def equalitiesHeading = P( "Equations:" )

  def inequalitiesHeading = P( "Inequalities:" )

  // Porta format, can contain equalities
  def portaConstraintsHeading = P( "INEQUALITY_SECTION" )

  type Header = (HPoly, Option[Seq[String]])

  def firstEqualitiesSection: P[Header] =
    P( equalitiesHeading ~ lineEndings ~ vecEquality ).flatMap { first =>
      (Pass ~ (lineEndings ~ vecEquality(first.lhs.length)).rep).map { rest =>
        require(first.op == EQ)
        require(rest.forall(_.op == EQ))
        val mAeq = tabulate(rest.size + 1, first.lhs.length) { (r, c) =>
          if (r == 0) first.lhs(c) else rest(r - 1).lhs(c)
        }
        val vbeq = vec(first.rhs +: rest.map(_.rhs): _*)
        (HPolytope.fromEqualities(mAeq, vbeq), None)
      }
    }

  def firstInequalitiesSection: P[Header] =
    P( (inequalitiesHeading | portaConstraintsHeading) ~ lineEndings ~ vecInequality ).flatMap { first =>
      (Pass ~ (lineEndings ~ vecInequality(first.lhs.length)).rep).map { rest =>
        require(first.op == LE)
        require(rest.forall(_.op == LE))
        val mA = tabulate(rest.size + 1, first.lhs.length) { (r, c) =>
          if (r == 0) first.lhs(c) else rest(r - 1).lhs(c)
        }
        val vb = vec(first.rhs +: rest.map(_.rhs): _*)
        (HPolytope.fromInequalities(mA, vb), None)
      }
    }

  def hUnnamedHeader: Parser[Header] = unnamedHeader.map( dim => (HPolytope.full[Rational](dim): HPoly, None) )

  def hNamedHeader: Parser[Header] = namedHeader.map( seq => (HPolytope.full[Rational](seq.size): HPoly, Some(seq)) )

  def hHeader: P[Header] = P( hNamedHeader | hUnnamedHeader | firstEqualitiesSection | firstInequalitiesSection )

  // Support for named expressions

  def operator: P[ComparisonOp] = P("<=").map(x => LE) | P("=").map(x => EQ) | P(">=").map(x => GE)

  def namedConstraint(names: Seq[String]): Parser[VCons] = P( expr ~ operator ~ expr ).map {
    case (lhs, op, rhs) =>
      val newLhs = lhs.filterKeys(_ != "") - rhs.filterKeys(_ != "")
      val newRhs = rhs.getOrElse("", Rational.zero) + lhs.getOrElse("", Rational.zero)
      newLhs.keys.find(!names.contains(_)) match {
        case Some(key) => throw new IllegalArgumentException(s"Variable $key is not present in names")
        case None => LinearConstraint(tabulate(names.size)(k => newLhs.getOrElse(names(k), Rational.zero)), op, newRhs)
      }
  }

  def equalityConstraint(dim: Int, namesOption: Option[Seq[String]]): P[VCons] = namesOption match {
    case Some(names) => namedConstraint(names) | vecEquality(dim)
    case None => vecEquality(dim)
  }

  def inequalityConstraint(dim: Int, namesOption: Option[Seq[String]]): P[VCons] = namesOption match {
    case Some(names) => namedConstraint(names) | vecInequality(dim)
    case None => vecInequality(dim)
  }

  def equalityConstraints(dim: Int, namesOption: Option[Seq[String]]): P[Seq[VCons]] =
    P( equalitiesHeading ~ lineEndings ~ equalityConstraint(dim, namesOption).rep(sep = lineEndings) )

  def inequalityConstraints(dim: Int, namesOption: Option[Seq[String]]): P[Seq[VCons]] =
    P( inequalitiesHeading ~ lineEndings ~ inequalityConstraint(dim, namesOption).rep(sep = lineEndings) )

  def portaConstraints(dim: Int, namesOption: Option[Seq[String]]): P[Seq[VCons]] =
    P( portaConstraintsHeading ~ lineEndings ~ inequalityConstraint(dim, namesOption).rep(sep = lineEndings) )

  def constraintsPolytope(dim: Int, namesOption: Option[Seq[String]]): Parser[HPoly] =
    P( equalityConstraints(dim, namesOption) | inequalityConstraints(dim, namesOption) | portaConstraints(dim, namesOption) )
      .map( seq => HPolytope.fromLinearConstraints(dim, seq) )

  type Section = Either[Maps, HPoly]

  def constraintsSection(dim: Int, namesOption: Option[Seq[String]]): P[Section] =
    constraintsPolytope(dim, namesOption).map( poly => Right(poly) )

  def section(dim: Int, namesOption: Option[Seq[String]]): P[Section] =
    P( constraintsSection(dim, namesOption) | mapsSection(namesOption) )

  def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps, HPoly)] =
    (section(dim, namesOption) ~ sectionEnd).rep.map { eithers =>
      val (mapsSeq, hpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val hpoly = hpolys.foldLeft(HPolytope.full[Rational](dim): HPoly) { (x, y) => x intersect y }
      (maps, hpoly)
    }

  def data = (hHeader ~ sectionEnd).flatMap {
    case (hpoly, namesOption) => (Pass ~ sections(hpoly.dim, namesOption) ~ End).map {
      case (maps, newHpoly) => HData(hpoly intersect newHpoly, namesOption, maps)
    }
  }

}
