package com.faacets
package polyta
package formats
package sympol

import spire.algebra.Group
import spire.math.SafeLong

import net.alasc.finite.Grp
import net.alasc.perms.{Cycle, Perm}
import fastparse.noApi._
import net.alasc.perms.default._
import net.alasc.syntax.all._

class SymmetryInfoRead extends FormatRead[SymmetryInfo] with SympolParsers {

    def firstPartOrder: P[SafeLong] =
      P( "generators for Aut(P) of order" ~/ positiveSafeLong ~ ":" )

    def firstPartCycle: P[Cycle] =
      P( "(" ~ positiveInt.rep(sep=",") ~ ")" ).map( seq => Cycle(seq.map(_ - 1): _*) )

    def firstPartPerm: P[Perm] = firstPartCycle.rep(min=1)
      .map( seq => Group[Perm].combine(seq.map(_.toPermutation[Perm])) )

    type Order = SafeLong

    type Generators = Seq[Perm]

    type Base = Seq[Int]

    def firstPart: P[(Order, Generators)] =
      P( firstPartOrder ~ lineEnding ~ firstPartPerm.rep(sep=lineEnding) )

    def secondPartHeader = "SymPol format:"

    def secondPart: P[(Generators, Base)] = P( secondPartHeader ~/ lineEnding ~ generators ~ base )

    def data = P(comments(firstPartOrder) ~ firstPart ~ lineEndings ~ secondPart ~ End).map {
      case (o, g1, (g2, b)) =>
        if (Grp(g1: _*) === Grp(g2: _*))
          SymmetryInfo(false, Some(o), g1, b)
        else
          throw new IllegalArgumentException("Different groups are provided in the two notations.")
    }
    
}
