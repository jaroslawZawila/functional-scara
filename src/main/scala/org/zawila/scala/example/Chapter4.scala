package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}
import org.zawila.scala.example

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =  flatMap(a => if (f(a)) Some(a) else None)


}

object Option {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a flatMap(a => b.map(b => f(a,b)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x),y)(_ :: _))

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Chapter4 {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) example.None else Some(xs.sum / xs.size)

   mean(xs) flatMap(m => Some(xs.map(x => math.pow(x - m, 2)).sum)) flatMap(x => if (xs.size - 1 > 0) Some(x / (xs.size -1)) else None)

  }



}

class Chapter4Test extends FunSpec with Matchers {
  import Chapter4._

  it("map") {
    Some(1).map(x => x * 5) should be(Some(5))
    None.map(_ => ()) should be(None)
  }

  it("flatMap") {
    Some(1).flatMap(x => Some(x.toString)) should be(Some("1"))
    Some(1).flatMap(_ => None) should be(None)
  }

  it("getOrElse") {
    Some(1).getOrElse(3) should be(1)
    None.getOrElse(3) should be(3)
  }

  it("orElse") {
    Some(1).orElse(None) should be(Some(1))
    None.orElse(Some(1)) should be(Some(1))
  }

  it("filter") {
    Some(1).filter(x => x > 0) should be(Some(1))
    Some(1).filter(x => x < 0) should be(None)
  }

  it("variance") {
    variance(Seq(17, 15, 23, 7, 9, 13)) should be(Some(33.2))
    variance(Seq(17)) should be(None)
    variance(Seq()) should be(None)
  }

  it("map2") {
    Option.map2(Some(2), Some(3))(_ * _) should be(Some(6))
    Option.map2[Int,Int,Int](Some(2), None)(_ * _) should be(None)
    Option.map2[Int,Int,Int](None, Some(3))(_ * _) should be(None)
  }

  it("sequance") {
    Option.sequence(List(Some(1), Some(2), Some(3))) should be( Some(List(1,2,3)) )
    Option.sequence(List(Some(1), None, Some(3))) should be(None)
  }

  it("traverse") {
    Option.traverse(List(1,2,3))(x => Some(x + 1)) should be(Some(List(2,3,4)))
  }

  val exception = new Exception()
  it("Either map") {
    Either.Try[Int]{1} map(_ + 2) should be(Right(3))
    Either.Try[Int]{throw exception} map(_ + 2) should be(Left(exception))
  }

  it("Either flatMap") {
    Either.Try[Int](1).flatMap(x => Either.Try(x + 1)) should be(Right(2))
    Either.Try[Int](throw exception).flatMap(x => Either.Try(x + 1)) should be(Left(exception))
  }

  it("Either orElse") {
    Either.Try[Int](1).orElse(Right(3)) should be(Right(1))
    Either.Try[Int](throw exception).orElse(Right(3)) should be(Right(3))
  }

  it("Eighter map2") {
    Either.Try(1).map2(Right(2))(_ + _) should be(Right(3))
    Either.Try[Int](throw exception).map2(Right(2))(_ + _) should be(Left(exception))
  }
}


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case l @ Left(x) => l
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case l @ Left(_) => l
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r @ Right(x) => r
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(a => b.map(bb => f(a,bb)))
}

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]




