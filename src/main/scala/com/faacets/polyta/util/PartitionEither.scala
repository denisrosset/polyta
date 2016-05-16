package com.faacets
package polyta
package util

object PartitionEither {

  def apply[A, B](es: Seq[Either[A, B]]): (Seq[A], Seq[B]) =
    es.foldRight (Seq.empty[A], Seq.empty[B]) { case (e, (as, bs)) =>
      e.fold (a => (a +: as, bs), b => (as, b +: bs))
    }

}
