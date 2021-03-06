package org.zawila.scala.example.chapter5

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec


sealed trait Stream2[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def loop[A](stream: Stream2[A], l: List[A]): List[A] = stream match {
      case Empty => l
      case Cons(h, t) => loop(t(), h() :: l)
    }

    loop(this, List.empty).reverse
  }

  import Stream2._
  def take(n: Int): Stream2[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty )
    case _ => Empty
  }

  def drop(n: Int): Stream2[A] = this match {
    case Cons(h, t) if n > 1  => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream2[A] = this match {
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

  def takeWhile2(p: A => Boolean): Stream2[A] = foldRight(empty[A])((a, b) => {
    if (p(a)) cons(a,b) else empty
  })

  def headOption1(): Option[A] = foldRight(None: Option[A])((a,b) => Some(a))

  def map[B](f: A => B): Stream2[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream2[A] = foldRight(empty[A])((a, b) => {
    if(f(a)) {
      cons(a,b)
    } else {
      b
    }
  })

  def append[B>:A](s: => Stream2[B]): Stream2[B] = foldRight(s)((h, t) => cons(h,t))

  def flatMap[B](f: A => Stream2[B]): Stream2[B] = foldRight(empty[B])((a, b) => f(a).foldRight(b)((aa, bb) => cons(aa, bb)))

  def map2[B](f: A => B): Stream2[B] = unfold(this)(s => s match {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None})

  def take3(n: Int): Stream2[A] = unfold((this, n))(s => s match {
    case (Cons(h, t), nn) if nn > 0 => Some((h(), (t(), nn - 1)))
    case _ => None
  })

  def takeWhile3(f: A => Boolean): Stream2[A] = unfold(this)(s => s match {
    case Cons(h, t) if f(h()) => Some(h(), t())
    case _ => None
  })

  def zipWith[B, C](as: Stream2[B])(f: (A,B) => C): Stream2[C] = unfold((this, as))(s => s._1 match {
    case Cons(h1, t1) => {
      s._2 match {
        case Cons(h2, t2) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }
    }
    case _ => None
  })

  def zipAll[B](s2: Stream2[B]): Stream2[(Option[A],Option[B])] = unfold((this, s2))(s => s._1 match {
    case Cons(h1, t1) => s._2 match {
      case Cons(h2, t2) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => Some((Some(h1()), None), (t1(), Stream2.empty))
    }
    case _ => None
  })
}

case object Empty extends Stream2[Nothing]

case class Cons[+A](h: () => A, t: () => Stream2[A]) extends Stream2[A]

object Stream2 {
  def cons[A](hd: => A, tl: => Stream2[A]): Stream2[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream2[A] = Empty

  def apply[A](as: A*): Stream2[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream2[A] = {
    lazy val tail: Stream2[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream2[Int] = cons(n, from(n + 1))

  def fibs(): Stream2[Int] = {
    def loop(f0: Int, f1: Int): Stream2[Int] =
      cons(f0, loop(f1, f0+f1))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream2[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibs2(): Stream2[Int] = unfold((0, 1))(s => Some((s._1 , (s._2, s._2 + s._1))))

  def from2(n: Int): Stream2[Int] = unfold(n)(s => Some(s, s+ 1))

  def constant2[A](a: A): Stream2[A] = unfold(a)(s => Some(s,s))

}


class Stream2Test extends FunSpec with Matchers {

  it("Stream toList") {
    Stream2(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
    Stream2.empty[Int].toList should be(List.empty[Int])
  }

  it("Stream take") {
    Stream2(1, 2, 3, 4).take(2).toList should be(List(1, 2))
    Stream2(1, 2, 3, 4).take(0).toList should be(List())
  }

  it("Stream drop") {
    Stream2(1, 2, 3, 4).drop(2).toList should be(List(3, 4))
    Stream2(1, 2, 3, 4).drop(0).toList should be(List(1,2,3,4))
    Stream2(1, 2, 3, 4).drop(5).toList should be(List())
  }

  it("Stream takeWhile") {
    Stream2(1,2,3,4).takeWhile(_ <= 3).toList should be(List(1, 2, 3))
  }

  it("Stream ForAll") {
    Stream2(2,4,6,8,10).forAll(_ % 2 == 0) should be(true)
    Stream2(2,4,5,8,10).forAll(_ % 2 == 0) should be(false)
  }

  it("Stream takeWhile2") {
    Stream2(1,2,3,4).takeWhile2(_ <= 3).toList should be(List(1, 2, 3))
  }

  it("Head option") {
    Stream2(1,2,3,4).headOption1() should be(Some(1))
    Stream2.empty[Int].headOption1() should be(None)
  }

  it("Fold map") {
    Stream2(1,2,3,4).map(_ + 1).toList should be(List(2,3,4,5))
    Stream2.empty[Int].map(_ + 1).toList should be(List.empty[Int])
  }

  it("filter") {
    Stream2(1,2,3,4,5,6).filter(_ % 2 == 0).toList should be(List(2,4,6))
  }

  it("flatMap") {
    Stream2(1,2,3).flatMap((x => Stream2(x,x))).toList should be(List(1,1,2,2,3,3))
  }

  it("Append") {
    Stream2(1,2,3,4).append(Stream2(5)).toList should be(List(1,2,3,4,5))
  }

  it("Constant") {
    Stream2.constant(4).take(3).toList should be(List(4,4,4))
  }

  it("From") {
    Stream2.from(1).take(4).toList should be(List(1,2,3,4))
  }

  it("fibonacci") {
    Stream2.fibs().take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  it("Unfold") {
    Stream2.unfold(1)((p) => Some((p, p+1))).take(5).toList should be(List(1,2,3,4,5))
  }

  it("Constant2") {
    Stream2.constant2(4).take(3).toList should be(List(4,4,4))
  }

  it("From2") {
    Stream2.from2(1).take(4).toList should be(List(1,2,3,4))
  }

  it("fibonacci2") {
    Stream2.fibs2().take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  it("Fold map2") {
    Stream2(1,2,3,4).map2(_ + 1).toList should be(List(2,3,4,5))
    Stream2.empty[Int].map2(_ + 1).toList should be(List.empty[Int])
  }

  it("Stream take3") {
    Stream2(1, 2, 3, 4).take3(2).toList should be(List(1, 2))
    Stream2(1, 2, 3, 4).take3(0).toList should be(List())
  }

  it("Stream takeWhile3") {
    Stream2(1,2,3,4).takeWhile3(_ <= 3).toList should be(List(1, 2, 3))
    Stream2(1,2,3,4).takeWhile3(_ <= 6).toList should be(List(1, 2, 3, 4))
  }

  it("zipWith") {
    Stream2(1,2,3,4).zipWith(Stream2(1,2,3,4))((a, b) => a + b).toList should be(List(2,4,6,8))
  }

  it("zipAll") {
    Stream2(1,2,3).zipAll(Stream2(3,4,5)).toList should be(List((Some(1),Some(3)),(Some(2),Some(4)),(Some(3),Some(5))))
  }
}
