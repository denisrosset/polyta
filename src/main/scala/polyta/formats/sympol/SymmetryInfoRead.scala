package com.faacets
package polyta
package formats
package sympol

import spire.algebra.Group
import spire.syntax.eq._

import net.alasc.finite.Grp
import net.alasc.prep.PGrp.default._
import net.alasc.perms.{Cycle, Perm}
import net.alasc.syntax.all._

class SymmetryInfoRead extends FormatRead[SymmetryInfo] {

  object Parsers extends ParsersBase with SympolParsers {

    def firstPartOrder: Parser[BigInt] = ("generators for Aut(P) of order" ~> positiveBigInt) <~ ":"

    def firstPartCycle: Parser[Cycle] = ("(" ~> repsep(positiveInt, ",")) <~ ")" ^^ { seq => Cycle(seq.map(_ - 1): _*) }

    def firstPartPerm: Parser[Perm] = rep1(firstPartCycle) ^^ { seq => Group[Perm].combine(seq.map(_.to[Perm])) }

    type Order = BigInt

    type Generators = Seq[Perm]

    type Base = Seq[Int]

    def firstPart: Parser[(Order, Generators)] = (firstPartOrder <~ lineEnding) ~ repsep(firstPartPerm, lineEnding) ^^ {
      case o ~ g => (o, g)
    }

    def secondPartHeader = "SymPol format:"

    def secondPart: Parser[(Generators, Base)] = (secondPartHeader ~ lineEnding) ~> generators ~ base ^^ {
      case g ~ b => (g, b)
    }

    def data = phrase(comments(firstPartOrder) ~> (firstPart <~ lineEndings) ~ secondPart) into {
      case ~((o, g1), (g2, b)) =>
        if (Grp(g1: _*) === Grp(g2: _*))
          success(SymmetryInfo(false, Some(o), g1, b))
        else
          failure("Different groups are provided in the two notations.")
    }
    
  }

}
