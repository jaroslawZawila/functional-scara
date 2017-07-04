package org.zawila.scala.example.chapter5

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec


sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def loop[A](stream: Stream[A], l: List[A]): List[A] = stream match {
      case Empty => l
      case Cons(h, t) => loop(t(), h() :: l)
    }

    loop(this, List.empty).reverse
  }

  import Stream._
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty )
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => {
    if (p(a)) cons(a,b) else empty
  })

  def headOption1(): Option[A] = foldRight(None: Option[A])((a,b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => {
    if(f(a)) {
      cons(a,b)
    } else {
      b
    }
  })

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a).foldRight(b)((aa,bb) => cons(aa, bb)))



}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def loop(f0: Int, f1: Int): Stream[Int] =
      cons(f0, loop(f1, f0+f1))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}


class StreamTest extends FunSpec with Matchers {

  it("Stream toList") {
    Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
    Stream.empty[Int].toList should be(List.empty[Int])
  }

  it("Stream take") {
    Stream(1, 2, 3, 4).take(2).toList should be(List(1, 2))
    Stream(1, 2, 3, 4).take(0).toList should be(List())
  }

  it("Stream drop") {
    Stream(1, 2, 3, 4).drop(2).toList should be(List(3, 4))
    Stream(1, 2, 3, 4).drop(0).toList should be(List(1,2,3,4))
    Stream(1, 2, 3, 4).drop(5).toList should be(List())
  }

  it("Stream takeWhile") {
    Stream(1,2,3,4).takeWhile(_ <= 3).toList should be(List(1, 2, 3))
  }

  it("Stream ForAll") {
    Stream(2,4,6,8,10).forAll(_ % 2 == 0) should be(true)
    Stream(2,4,5,8,10).forAll(_ % 2 == 0) should be(false)
  }

  it("Stream takeWhile2") {
    Stream(1,2,3,4).takeWhile2(_ <= 3).toList should be(List(1, 2, 3))
  }

  it("Head option") {
    Stream(1,2,3,4).headOption1() should be(Some(1))
    Stream.empty[Int].headOption1() should be(None)
  }

  it("Fold map") {
    Stream(1,2,3,4).map(_ + 1).toList should be(List(2,3,4,5))
    Stream.empty[Int].map(_ + 1).toList should be(List.empty[Int])
  }

  it("filter") {
    Stream(1,2,3,4,5,6).filter(_ % 2 == 0).toList should be(List(2,4,6))
  }

  it("flatMap") {
    Stream(1,2,3).flatMap((x => Stream(x,x))).toList should be(List(1,1,2,2,3,3))
  }

  it("Append") {
    Stream(1,2,3,4).append(Stream(5)).toList should be(List(1,2,3,4,5))
  }

  it("Constant") {
    Stream.constant(4).take(3).toList should be(List(4,4,4))
  }

  it("From") {
    Stream.from(1).take(4).toList should be(List(1,2,3,4))
  }

  it("fibonacci") {
    Stream.fibs().take(7).toList should be(List(0,1,1,2,3,5,8))
  }
}
