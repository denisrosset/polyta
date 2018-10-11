package com.faacets.polyta

import scalin.immutable.Vec

case class LinearProgram[A](direction: Direction, objective: Vec[A], feasibleSet: HPolytope[A])
