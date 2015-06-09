package com.faacets
package polyta

import scala.{specialized => sp}

import scala.collection.{mutable, immutable}

import spire.algebra._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

object DomainOrbits {
  def orbits[G](group: Grp[G], representation: Representation[G]): Set[immutable.BitSet] = {
    import net.alasc.math.OrbitInstances._
    implicit val action = representation.action
    val rem = mutable.BitSet.empty ++= 0 until representation.size
    val orbits = Set.newBuilder[immutable.BitSet]
    while (rem.nonEmpty) {
      val orbit = immutable.BitSet(rem.head) <|+| group.generators
      orbits += orbit
      rem --= orbit
    }
    orbits.result
  }
}
