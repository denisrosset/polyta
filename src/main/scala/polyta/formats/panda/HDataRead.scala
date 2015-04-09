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

class HDataRead[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatRead[HData[M, V]] {

  type VCons = LinearConstraint[V, Rational]
  type HPoly = HPolyhedron[M, V, Rational]

  object Parser extends ParserBase with PandaDataParser[V] {
    implicit def M: MatVecInField[M, V, Rational] = HDataRead.this.M
    implicit def V: VecInField[V, Rational] = M.V

    /* A Panda file can be either named or unnamed.
     * 
     * Only files with variable names can have symmetry information.
     * 
     * If neither names nor dimension are provided, the dimension of unnamed
     * variables is guessed from the first Equations/Inequalities section.
     */

    def vecEquality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearEquality(V.build(seq.init:_*), -seq.last)
    }

    def vecEquality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearEquality(V.build(lhs: _*), -minusRhs)
      }

    def vecInequality: Parser[VCons] = rep(rational) ^^ { seq =>
      LinearInequalityLE(V.build(seq.init:_*), -seq.last)
    }

    def vecInequality(dim: Int): Parser[VCons] =
      (repN(dim, rational) ~ rational) ^^ {
        case lhs ~ minusRhs => LinearInequalityLE(V.build(lhs: _*), -minusRhs)
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
          val mAeq = M.vertcat(first.lhs.rowMat[M] +: rest.map(_.lhs.rowMat[M]): _*)
          val vbeq = V.build(first.rhs +: rest.map(_.rhs): _*)
          (HPolyhedron.fromEqualities(mAeq, vbeq), None)
        }
      }

    def firstInequalitiesSection: Parser[Header] =
      ((inequalitiesHeading | portaConstraintsHeading) ~ lineEndings) ~> vecInequality into { first =>
        rep(lineEndings ~> vecInequality(first.lhs.length)) ^^ { rest =>
          require(first.op == LE)
          require(rest.forall(_.op == LE))
          val mA = M.vertcat(first.lhs.rowMat[M] +: rest.map(_.lhs.rowMat[M]): _*)
          val vb = V.build(first.rhs +: rest.map(_.rhs): _*)
          (HPolyhedron.fromInequalities(mA, vb), None)
        }
      }

    def hUnnamedHeader: Parser[Header] = unnamedHeader ^^ { dim =>
      (HPolyhedron.empty[M, V, Rational](dim), None)
    }

    def hNamedHeader: Parser[Header] = namedHeader ^^ { seq =>
      (HPolyhedron.empty(seq.size), Some(seq))
    }

    def hHeader: Parser[Header] = hNamedHeader | hUnnamedHeader | firstEqualitiesSection | firstInequalitiesSection

    // Support for named expressions

    def onlyVariable: Parser[(String, Rational)] = variable ^^ { str => (str, Rational.one) }

    def onlyCoefficient: Parser[(String, Rational)] = nonNegativeRational ^^ { rat => ("", rat) }

    def coefficientAndVariable: Parser[(String, Rational)] = nonNegativeRational ~ variable ^^ {
      case rat ~ str => (str, rat)
    }

    def positiveTerm: Parser[(String, Rational)] =
      coefficientAndVariable | onlyVariable | onlyCoefficient

    def firstTerm: Parser[(String, Rational)] = opt(sign) ~ positiveTerm ^^ {
      case ~(Some(-1), (str, rat)) => (str, -rat)
      case other ~ term => term
    }

    def nextTerm: Parser[(String, Rational)] = sign ~ positiveTerm ^^ {
      case ~(-1, (str, rat)) => (str, -rat)
      case other ~ term => term
    }

    def expr: Parser[Map[String, Rational]] = firstTerm ~ rep(nextTerm) ^^ {
      case first ~ next => (Map(first._1 -> first._2) /: next) {
        case (map, (v, r)) => Map(v -> r) + map
      }
    }

    def operator: Parser[ComparisonOperator] = ("<=" ^^^ LE) | ("=" ^^^ EQ) | (">=" ^^^ GE)

    def namedConstraint(names: Seq[String]): Parser[VCons] = expr ~ operator ~ expr into {
      case lhs ~ op ~ rhs =>
        val newLhs = lhs.filterKeys(_ != "") - rhs.filterKeys(_ != "")
        val newRhs = rhs.getOrElse("", Rational.zero) + lhs.getOrElse("", Rational.zero)
        newLhs.keys.find(!names.contains(_)) match {
          case Some(key) => failure(s"Variable $key is not present in names")
          case None => success(LinearConstraint(
            V.fromFunV(new FunV[Rational] {
              def len = names.size
              def f(k: Int): Rational = newLhs.getOrElse(names(k), Rational.zero)
            }), op, newRhs
          ))
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
      HPolyhedron.fromLinearConstraints(dim, seq)
    }

    type Section = Either[Maps, HPoly]

    def constraintsSection(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] =
      constraintsPolyhedron(dim, namesOption) ^^ { poly => Right(poly) }

    def section(dim: Int, namesOption: Option[Seq[String]]): Parser[Section] = constraintsSection(dim, namesOption) | mapsSection(namesOption)

    def sections(dim: Int, namesOption: Option[Seq[String]]): Parser[(Maps, HPoly)] = rep(section(dim, namesOption) <~ sectionEnd) ^^ { eithers =>
      val (mapsSeq, hpolys) = util.PartitionEither(eithers)
      val maps = mapsSeq.flatten
      val hpoly = HPolyhedron.intersection((HPolyhedron.empty[M, V, Rational](dim) +: hpolys): _*)
      (maps, hpoly)
    }

    def data = phrase((hHeader <~ sectionEnd) into {
      case (hpoly, namesOption) => sections(hpoly.nX, namesOption) ^^ {
        case (maps, newHpoly) => HData(HPolyhedron.intersection(hpoly, newHpoly), namesOption, maps)
      }
    })
  }
}
