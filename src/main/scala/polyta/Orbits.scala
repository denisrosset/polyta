package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.action._

import net.alasc.algebra._

object Orbits {

  def orbit[G](i: Int, generators: Iterable[G], action: Action[Int, G]): scala.collection.immutable.BitSet = {
  import scala.collection.mutable.BitSet
    var res = BitSet(i)
    var toCheck = BitSet(i)
    var newAdded = BitSet(i)
    while (!toCheck.isEmpty) {
      val kIt = toCheck.iterator
      while (kIt.hasNext) {
        val k = kIt.next
        val gIt = generators.iterator
        while (gIt.hasNext) {
          val g = gIt.next
          val image = action.actr(k, g)
          if (!res.contains(image)) {
            res += image
            newAdded += image
          }
        }
      }
      val temp = toCheck
      toCheck = newAdded
      newAdded = temp
      newAdded.clear
    }
    res.toImmutable
  }

  def orbits[G](size: Int, generators: Iterable[G], action: Action[Int, G]): Set[scala.collection.immutable.BitSet] = {
    import net.alasc.math.OrbitInstances._
    implicit val action0 = action
    val rem = mutable.BitSet.empty ++= 0 until size
    val orbits = Set.newBuilder[immutable.BitSet]
    while (rem.nonEmpty) {
      val curOrbit = orbit(rem.head, generators, action)
      orbits += curOrbit
      rem --= curOrbit
    }
    orbits.result
  }

}
