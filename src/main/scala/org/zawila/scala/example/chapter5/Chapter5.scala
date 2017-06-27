package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}


sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def loop[A](stream: Stream[A], l: List[A]): List[A] = stream match {
      case Empty => l
      case Cons(h, t) => loop(t(), h() :: l)
    }

    loop(this, List.empty)
  }
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

  it("Strem toList") {
    Stream(1,2,3,4).toList should be(List(1,2,3,4))
  }
}
