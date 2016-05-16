package com.faacets
package polyta

sealed trait ComparisonOp

sealed trait InequalityOp extends ComparisonOp

object ComparisonOp {

  case object EQ extends ComparisonOp
  case object LE extends InequalityOp
  case object GE extends InequalityOp

}
