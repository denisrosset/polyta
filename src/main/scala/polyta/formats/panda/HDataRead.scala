package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.std.map._
import spire.syntax.field._

import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}
import scalin.immutable.dense._
import scalin.syntax.all._

import ComparisonOp._

class HDataRead extends FormatRead[HData] {

  type VCons = LinearConstraint[Rational]
  type HPoly = HPolytopeM[Rational]

  object Parsers extends ParsersBase with PandaDataParsers with NamedExprParsers {

    /* A Panda file can be either named or unnamed.
     * 
     * Only files with variable names can have symmetry information.
     * 
     * If neither names nor dimension are provided, the dimension of unnamed
     * variables is guessed from the first Equations/Inequalities section.
     */

    def vecEquality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearEquality(vec(seq.init:_*), -seq.last)
    }

    def vecEquality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearEquality(vec(lhs: _*), -minusRhs)
      }

    def vecInequality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearInequality(vec(seq.init:_*), LE, -seq.last)
    }

    def vecInequality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearInequality(vec(lhs: _*), LE, -minusRhs)
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
          val mAeq = IMat.tabulate(rest.size + 1, first.lhs.length) { (r, c) =>
            if (r == 0) first.lhs(c) else rest(r - 1).lhs(c)
          }
          val vbeq = vec(first.rhs +: rest.map(_.rhs): _*)
          (HPolytopeM.fromEqualities(mAeq, vbeq), None)
        }
      }

    def firstInequalitiesSection: Parser[Header] =
      ((inequalitiesHeading | portaConstraintsHeading) ~ lineEndings) ~> vecInequality into { first =>
        rep(lineEndings ~> vecInequality(first.lhs.length)) ^^ { rest =>
          require(first.op == LE)
          require(rest.forall(_.op == LE))
          val mA = IMat.tabulate(rest.size + 1, first.lhs.length) { (r, c) =>
            if (r == 0) first.lhs(c) else rest(r - 1).lhs(c)
          }
          val vb = vec(first.rhs +: rest.map(_.rhs): _*)
          (HPolytopeM.fromInequalities(mA, vb), None)
        }
      }

    def hUnnamedHeader: Parser[Header] = unnamedHeader ^^ { dim =>
      (HPolytopeM.empty(dim), None)
    }

    def hNamedHeader: Parser[Header] = namedHeader ^^ { seq =>
      (HPolytopeM.empty(seq.size), Some(seq))
    }

    def hHeader: Parser[Header] = hNamedHeader | hUnnamedHeader | firstEqualitiesSection | firstInequalitiesSection

    // Support for named expressions

    def operator: Parser[ComparisonOp] = ("<=" ^^^ LE) | ("=" ^^^ EQ) | (">=" ^^^ GE)

    def namedConstraint(names: Seq[String]): Parser[VCons] = expr ~ operator ~ expr into {
      case lhs ~ op ~ rhs =>
        val newLhs = lhs.filterKeys(_ != "") - rhs.filterKeys(_ != "")
        val newRhs = rhs.getOrElse("", Rational.zero) + lhs.getOrElse("", Rational.zero)
        newLhs.keys.find(!names.contains(_)) match {
          case Some(key) => failure(s"Variable $key is not present in names")
          case None => success(LinearConstraint(IVec.tabulate(names.size)(k => newLhs.getOrElse(names(k), Rational.zero)), op, newRhs))
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

    def constraintsPolytope(dim: Int, namesOption: Option[Seq[String]]): Parser[HPoly] = (equalityConstraints(dim, namesOption) | inequalityConstraints(dim, namesOption) | portaConstraints(dim, namesOption)) ^^ { seq =>
      HPolytopeM.fromLinearConstraints(dim, seq)
    }

    type Section = Either[Maps, HPoly]

    def constraintsSection(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] =
      constraintsPolytope(dim, namesOption) ^^ { poly => Right(poly) }

    def section(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] = constraintsSection(dim, namesOption) | mapsSection(namesOption)

    def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps, HPoly)] = rep(section(dim, namesOption) <~ sectionEnd) ^^ { eithers =>
      val (mapsSeq, hpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val hpoly = hpolys.foldLeft(HPolytopeM.empty[Rational](dim)) { (x, y) => HPolytopeM.WithoutSym.intersection(x, y) }
      (maps, hpoly)
    }

    def data = phrase((hHeader <~ sectionEnd) into {
      case (hpoly, namesOption) => sections(hpoly.dim, namesOption) ^^ {
        case (maps, newHpoly) => HData(HPolytopeM.WithoutSym.intersection(hpoly, newHpoly), namesOption, maps)
      }
    })
  }

}
