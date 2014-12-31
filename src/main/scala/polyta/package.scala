package com.faacets

import spire.algebra._
import spire.math.Rational
import spire.util.Nullbox

import qalg.immutable.QVector
import net.alasc.algebra._

package object polyta {
  implicit object QVectorSequence extends Sequence[QVector, Rational] {
    def length(qv: QVector) = qv.length
    def elemAt(qv: QVector, i: Int): Rational = qv(i)
    def toIndexedSeq(qv: QVector): IndexedSeq[Rational] = qv.toIndexedSeq
  }
  final class QVectorPermutationAction[P: FiniteGroup: FaithfulPermutationAction] extends NullboxPartialAction[QVector, P]{
    import net.alasc.syntax.permutationAction._
    import spire.syntax.group._
    import spire.syntax.action._

    override def actlIsDefined(p: P, qv: QVector) = p.supportMax.getOrElseFast(-1) < qv.length
    override def actrIsDefined(qv: QVector, p: P) = p.supportMax.getOrElseFast(-1) < qv.length

    def partialActl(p: P, qv: QVector): Nullbox[QVector] =
      if (p.supportMax.getOrElseFast(-1) >= qv.length) Nullbox.empty[QVector] else
        Nullbox(QVector.tabulate(qv.length)( i => qv(i <|+| p) ))

    def partialActr(qv: QVector, p: P): Nullbox[QVector] = partialActl(p.inverse, qv)

  }
}
