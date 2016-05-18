package com.faacets.polyta
package solvers

import spire.math.Rational

import org.scalatest.{FunSuite, Matchers}

class SympolTest extends FunSuite with Matchers {

  test("Order of cube symmetry group (V)") {
    val cv = gallery.Cube.inV[Rational](3)
    import Sympol.FindSymmetriesV
    (cv: VPolytope.Aux[Rational, Symmetry.Combinatorial]).symmetry.group.order shouldBe 48
  }

  test("Order of cube symmetry group (H)") {
    val ch = gallery.Cube.inH[Rational](3)
    import Sympol.FindSymmetriesH
    (ch: HPolytope.Aux[Rational, Symmetry.Combinatorial]).symmetry.group.order shouldBe 48
  }

}
