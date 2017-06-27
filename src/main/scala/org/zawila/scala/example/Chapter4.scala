package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}
import org.zawila.scala.example

sealed trait Option2[+A] {
  def map[B](f: A => B): Option2[B] = this match {
    case Some2(x) => Some2(f(x))
    case None2 => None2
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some2(x) => x
    case None2 => default
  }

  def flatMap[B](f: A => Option2[B]): Option2[B] = this.map(f).getOrElse(None2)

  def orElse[B >: A](ob: => Option2[B]): Option2[B] = this map (Some2(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option2[A] =  flatMap(a => if (f(a)) Some2(a) else None2)


}

object Option2 {

  def sequence[A](a: List[Option2[A]]): Option2[List[A]] = traverse(a)(x => x)

  def map2[A, B, C](a: Option2[A], b: Option2[B])(f: (A,B) => C): Option2[C] = a flatMap(a => b.map(b => f(a,b)))

  def traverse[A, B](a: List[A])(f: A => Option2[B]): Option2[List[B]] = a.foldRight[Option2[List[B]]](Some2(Nil))((x, y) => map2(f(x),y)(_ :: _))

}

case class Some2[+A](get: A) extends Option2[A]
case object None2 extends Option2[Nothing]

object Chapter4 {
  def variance(xs: Seq[Double]): Option2[Double] = {
    def mean(xs: Seq[Double]): Option2[Double] = if (xs.isEmpty) example.None2 else Some2(xs.sum / xs.size)

   mean(xs) flatMap(m => Some2(xs.map(x => math.pow(x - m, 2)).sum)) flatMap(x => if (xs.size - 1 > 0) Some2(x / (xs.size -1)) else None2)

  }



}

class Chapter4Test extends FunSpec with Matchers {
  import Chapter4._

  it("map") {
    Some2(1).map(x => x * 5) should be(Some2(5))
    None2.map(_ => ()) should be(None2)
  }

  it("flatMap") {
    Some2(1).flatMap(x => Some2(x.toString)) should be(Some2("1"))
    Some2(1).flatMap(_ => None2) should be(None2)
  }

  it("getOrElse") {
    Some2(1).getOrElse(3) should be(1)
    None2.getOrElse(3) should be(3)
  }

  it("orElse") {
    Some2(1).orElse(None2) should be(Some2(1))
    None2.orElse(Some2(1)) should be(Some2(1))
  }

  it("filter") {
    Some2(1).filter(x => x > 0) should be(Some2(1))
    Some2(1).filter(x => x < 0) should be(None2)
  }

  it("variance") {
    variance(Seq(17, 15, 23, 7, 9, 13)) should be(Some2(33.2))
    variance(Seq(17)) should be(None2)
    variance(Seq()) should be(None2)
  }

  it("map2") {
    Option2.map2(Some2(2), Some2(3))(_ * _) should be(Some2(6))
    Option2.map2[Int,Int,Int](Some2(2), None2)(_ * _) should be(None2)
    Option2.map2[Int,Int,Int](None2, Some2(3))(_ * _) should be(None2)
  }

  it("sequance") {
    Option2.sequence(List(Some2(1), Some2(2), Some2(3))) should be( Some2(List(1,2,3)) )
    Option2.sequence(List(Some2(1), None2, Some2(3))) should be(None2)
  }

  it("traverse") {
    Option2.traverse(List(1,2,3))(x => Some2(x + 1)) should be(Some2(List(2,3,4)))
  }

  val exception = new Exception()
  it("Either map") {
    Either2.Try[Int]{1} map(_ + 2) should be(Right2(3))
    Either2.Try[Int]{throw exception} map(_ + 2) should be(Left2(exception))
  }

  it("Either flatMap") {
    Either2.Try[Int](1).flatMap(x => Either2.Try(x + 1)) should be(Right2(2))
    Either2.Try[Int](throw exception).flatMap(x => Either2.Try(x + 1)) should be(Left2(exception))
  }

  it("Either orElse") {
    Either2.Try[Int](1).orElse(Right2(3)) should be(Right2(1))
    Either2.Try[Int](throw exception).orElse(Right2(3)) should be(Right2(3))
  }

  it("Eighter map2") {
    Either2.Try(1).map2(Right2(2))(_ + _) should be(Right2(3))
    Either2.Try[Int](throw exception).map2(Right2(2))(_ + _) should be(Left2(exception))
  }
}


sealed trait Either2[+E, +A] {
  def map[B](f: A => B): Either2[E, B] = this match {
    case Right2(x) => Right2(f(x))
    case l @ Left2(x) => l
  }
  def flatMap[EE >: E, B](f: A => Either2[EE, B]): Either2[EE, B] = this match {
    case Right2(x) => f(x)
    case l @ Left2(_) => l
  }
  def orElse[EE >: E,B >: A](b: => Either2[EE, B]): Either2[EE, B] = this match {
    case r @ Right2(x) => r
    case Left2(_) => b
  }
  def map2[EE >: E, B, C](b: Either2[EE, B])(f: (A, B) => C): Either2[EE, C] = this.flatMap(a => b.map(bb => f(a,bb)))
}

object Either2 {
  def Try[A](a: => A): Either2[Exception, A] =
    try Right2(a)
    catch { case e: Exception => Left2(e) }
}

case class Left2[+E](value: E) extends Either2[E, Nothing]
case class Right2[+A](value: A) extends Either2[Nothing, A]




