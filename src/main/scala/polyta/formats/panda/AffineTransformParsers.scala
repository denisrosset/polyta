package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

import scalin.immutable.Vec
import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}

object AffineTransformParsers extends NamedExprParsers {

  override def skipWhitespace = false

  def image(names: Seq[String]): Parser[(Vec[Rational], Rational)] =
    expr into { terms =>
      val constant = terms.getOrElse("", Rational.zero)
      val coeffs = terms.filterKeys(_ != "")
      coeffs.keys.find(!names.contains(_)) match {
        case Some(key) => failure(s"Variable $key is not present in names")
        case None => success((IVec.tabulate(names.size)(k => coeffs.getOrElse(names(k), Rational.zero)), constant))
      }
    }

  def repNsep[A](n: Int, p: => Parser[A], sep: => Parser[Any]): Parser[List[A]] =
    if (n == 0) success(Nil)
    else if (n == 1) ( p ^^ { List(_) } )
    else ( (p ~ repN(n - 1, sep ~> p)) ^^ { case x ~ xs => x :: xs } )

  def terms(names: Seq[String]): Parser[AffineTransform[Rational]] = {
    val dim = names.size
    repNsep(dim, image(names), rep1(" ")) ^^ { images =>
      val vb = IVec.tabulate(dim)(images(_)._2)
      val mA = IMat.tabulate(images.size, dim)( (r, c) => images(r)._1(c) )
      AffineTransform[Rational](mA, vb)
    }
  }

}
