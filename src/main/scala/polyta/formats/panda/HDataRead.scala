package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.std.map._
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.math.Perm

class HDataRead[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends FormatRead[HData[M, V]] {

  type VCons = LinearConstraint[V, Rational]
  type HPoly = HPolyhedronM[M, V, Rational]

  object Parsers extends ParsersBase with PandaDataParsers[V] with NamedExprParsers {
    implicit def alg = HDataRead.this.alg
    /* A Panda file can be either named or unnamed.
     * 
     * Only files with variable names can have symmetry information.
     * 
     * If neither names nor dimension are provided, the dimension of unnamed
     * variables is guessed from the first Equations/Inequalities section.
     */

    def vecEquality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearEquality(VecBuilder[V, Rational].build(seq.init:_*), -seq.last)
    }

    def vecEquality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearEquality(VecBuilder[V, Rational].build(lhs: _*), -minusRhs)
      }

    def vecInequality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearInequalityLE(VecBuilder[V, Rational].build(seq.init:_*), -seq.last)
    }

    def vecInequality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearInequalityLE(VecBuilder[V, Rational].build(lhs: _*), -minusRhs)
      }

    def equalitiesHeading = "Equations:"

    def inequalitiesHeading = "Inequalities:"

    // Porta format, can contain equalities
    def portaConstraintsHeading = "INEQUALITY_SECTION" 

    type Header = (HPoly, Option[Seq[String]])

    def firstEqualitiesSection: Parser[Header] =
      (equalitiesHeading ~ lineEndings) ~> vecEquality into { first =>
        rep(lineEndings ~> vecEquality(first.lhs.length)) ^^ { rest =>
          require(first.op == EQ)
          require(rest.forall(_.op == EQ))
          val mAeq = MatBuilder[M, Rational].fromRows(first.lhs.length, first.lhs +: rest.map(_.lhs): _*)
          val vbeq = VecBuilder[V, Rational].build(first.rhs +: rest.map(_.rhs): _*)
          (HPolyhedronM.fromEqualities(mAeq, vbeq), None)
        }
      }

    def firstInequalitiesSection: Parser[Header] =
      ((inequalitiesHeading | portaConstraintsHeading) ~ lineEndings) ~> vecInequality into { first =>
        rep(lineEndings ~> vecInequality(first.lhs.length)) ^^ { rest =>
          require(first.op == LE)
          require(rest.forall(_.op == LE))
          val mA = MatBuilder[M, Rational].fromRows(first.lhs.length, first.lhs +: rest.map(_.lhs): _*)
          val vb = VecBuilder[V, Rational].build(first.rhs +: rest.map(_.rhs): _*)
          (HPolyhedronM.fromInequalities(mA, vb), None)
        }
      }

    def hUnnamedHeader: Parser[Header] = unnamedHeader ^^ { dim =>
      (HPolyhedronM.empty[M, V, Rational](dim), None)
    }

    def hNamedHeader: Parser[Header] = namedHeader ^^ { seq =>
      (HPolyhedronM.empty(seq.size), Some(seq))
    }

    def hHeader: Parser[Header] = hNamedHeader | hUnnamedHeader | firstEqualitiesSection | firstInequalitiesSection

    // Support for named expressions

    def operator: Parser[ComparisonOperator] = ("<=" ^^^ LE) | ("=" ^^^ EQ) | (">=" ^^^ GE)

    def namedConstraint(names: Seq[String]): Parser[VCons] = expr ~ operator ~ expr into {
      case lhs ~ op ~ rhs =>
        val newLhs = lhs.filterKeys(_ != "") - rhs.filterKeys(_ != "")
        val newRhs = rhs.getOrElse("", Rational.zero) + lhs.getOrElse("", Rational.zero)
        newLhs.keys.find(!names.contains(_)) match {
          case Some(key) => failure(s"Variable $key is not present in names")
          case None => success(LinearConstraint(VecBuilder[V, Rational].tabulate(names.size)(k => newLhs.getOrElse(names(k), Rational.zero)), op, newRhs))
        }
    }

    def equalityConstraint(dim: Int, namesOption: Option[Seq[String]]): Parser[VCons] = namesOption match {
      case Some(names) => namedConstraint(names) | vecEquality(dim)
      case None => vecEquality(dim)
    }

    def inequalityConstraint(dim: Int, namesOption: Option[Seq[String]]): Parser[VCons] = namesOption match {
      case Some(names) => namedConstraint(names) | vecInequality(dim)
      case None => vecInequality(dim)
    }

    def equalityConstraints(dim: Int, namesOption: Option[Seq[String]]): Parser[Seq[VCons]] = (equalitiesHeading ~ lineEndings) ~> repsep(equalityConstraint(dim, namesOption), lineEndings)

    def inequalityConstraints(dim: Int, namesOption: Option[Seq[String]]): Parser[Seq[VCons]] = (inequalitiesHeading ~ lineEndings) ~> repsep(inequalityConstraint(dim, namesOption), lineEndings)

    def portaConstraints(dim: Int, namesOption: Option[Seq[String]]): Parser[Seq[VCons]] = (portaConstraintsHeading ~ lineEndings) ~> repsep(inequalityConstraint(dim, namesOption), lineEndings)

    def constraintsPolyhedron(dim: Int, namesOption: Option[Seq[String]]): Parser[HPoly] = (equalityConstraints(dim, namesOption) | inequalityConstraints(dim, namesOption) | portaConstraints(dim, namesOption)) ^^ { seq =>
      HPolyhedronM.fromLinearConstraints(dim, seq)
    }

    type Section = Either[Maps[M, V], HPoly]

    def constraintsSection(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] =
      constraintsPolyhedron(dim, namesOption) ^^ { poly => Right(poly) }

    def section(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] = constraintsSection(dim, namesOption) | mapsSection(namesOption)

    def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps[M, V], HPoly)] = rep(section(dim, namesOption) <~ sectionEnd) ^^ { eithers =>
      val (mapsSeq, hpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val hpoly = HPolyhedronM.intersection((HPolyhedronM.empty[M, V, Rational](dim) +: hpolys): _*)
      (maps, hpoly)
    }

    def data = phrase((hHeader <~ sectionEnd) into {
      case (hpoly, namesOption) => sections(hpoly.nX, namesOption) ^^ {
        case (maps, newHpoly) => HData(HPolyhedronM.intersection(hpoly, newHpoly), namesOption, maps)
      }
    })
  }
}
