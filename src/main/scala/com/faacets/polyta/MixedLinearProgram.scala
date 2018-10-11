package com.faacets.polyta

import scalin.immutable.Vec

case class MixedLinearProgram[A](direction: Direction, objective: Vec[A], feasibleSet: HPolytope[A], integerVariables: Set[Int])
