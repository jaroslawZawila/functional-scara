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

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???
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
}
