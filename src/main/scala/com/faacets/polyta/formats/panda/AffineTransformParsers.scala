package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

import fastparse.WhitespaceApi
import scalin.immutable.Vec
import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}

object NoWhitespace {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    Fail
  }

}

object AffineTransformParsers extends NamedExprParsers {

  import fastparse.noApi._

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    NoWhitespace.White.parserApi(p0)

  def image(names: Seq[String]): P[(Vec[Rational], Rational)] =
    expr.map { terms =>
      val constant = terms.getOrElse("", Rational.zero)
      val coeffs = terms.filterKeys(_ != "")
      coeffs.keys.find(!names.contains(_)) match {
        case Some(key) => throw new IllegalArgumentException(s"Variable $key is not present in names")
        case None => (IVec.tabulate(names.size)(k => coeffs.getOrElse(names(k), Rational.zero)), constant)
      }
    }

  def terms(names: Seq[String]): P[AffineTransform[Rational]] = {
    val dim = names.size
    image(names).rep(min=dim, max=dim, sep=" ".rep(1)).map { images =>
      val vb = IVec.tabulate(dim)(images(_)._2)
      val mA = IMat.tabulate(images.size, dim)( (r, c) => images(r)._1(c) )
      AffineTransform[Rational](mA, vb)
    }
  }

}
