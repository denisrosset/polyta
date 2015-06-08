package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

class AffineTransformParsers[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends NamedExprParsers {

  override def skipWhitespace = false
  def image(names: Seq[String]): Parser[(V, Rational)] =
    expr into { terms =>
      val constant = terms.getOrElse("", Rational.zero)
      val coeffs = terms.filterKeys(_ != "")
      coeffs.keys.find(!names.contains(_)) match {
        case Some(key) => failure(s"Variable $key is not present in names")
        case None => success((VecBuilder[V, Rational].tabulate(names.size)(k => coeffs.getOrElse(names(k), Rational.zero)), constant))
      }
    }

  def repNsep[A](n: Int, p: => Parser[A], sep: => Parser[Any]): Parser[List[A]] =
    if (n == 0) success(Nil)
    else if (n == 1) ( p ^^ { List(_) } )
    else ( (p ~ repN(n - 1, sep ~> p)) ^^ { case x ~ xs => x :: xs } )

  def terms(names: Seq[String]): Parser[AffineTransform[M, V, Rational]] = {
    val dim = names.size
    repNsep(dim, image(names), rep1(" ")) ^^ { images =>
      val vb = VecBuilder[V, Rational].tabulate(dim)(images(_)._2)
      val mA = MatBuilder[M, Rational].fromRows(dim, images.map(_._1): _*)
      AffineTransform[M, V, Rational](mA, vb)
    }
  }
}
