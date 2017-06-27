package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

sealed trait List2[+A] // `List` data type, parameterized on a type, `A`
case object Nil2 extends List2[Nothing] // A `List` data Cons2tructor representing the empty list
/* Another data Cons2tructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons2`.
 */
case class Cons2[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List2[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil2 => 0 // The sum of the empty list is 0.
    case Cons2(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List2[Double]): Double = ds match {
    case Nil2 => 1.0
    case Cons2(0.0, _) => 0.0
    case Cons2(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List2[A] = // Variadic function syntax
    if (as.isEmpty) Nil2
    else Cons2(as.head, apply(as.tail: _*))

  def tail[A](l: List2[A]): List2[A] =  l match {
    case Nil2 => Nil2
    case Cons2(_, t) => t
  }

  def setHead[A](list: List2[A], first: A) = list match {
    case Nil2 => List2(first)
    case Cons2(x, t) => Cons2(first, t)
  }

  def drop[A](l: List2[A], idx: Int): List2[A] = {
    def loop(l: List2[A], index:Int): List2[A] = index match {
      case x if x < idx => loop(tail(l), index + 1)
      case _ => l
    }
    loop(l, 0)
  }

  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = l match {
    case Nil2 => Nil2
    case Cons2(x, xs) => if (f(x)) l else dropWhile(tail(l), f)
  }

  def init[A](list: List2[A]): List2[A] = list match {
      case Nil2 => Nil2
      case Cons2(x, Nil2) => Nil2
      case Cons2(x,xs) => Cons2(x, init(xs))
  }

  def foldRight[A,B](as: List2[A], z: B)(f: (A, B) => B): B = as match {
    case Nil2 => z
    case Cons2(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List2[Int]) = {
    foldRight(ns, 0)(_ + _)
  }

  def product2(ns: List2[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  def lengths[A](l: List2[A]): Int = foldRight(l, 0)((_, c) => c + 1)

  def foldLeft[A, B](as: List2[A], z: B)(f: (B, A) => B): B = as match {
    case Nil2 => z
    case Cons2(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List2[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def lengths2[A](l: List2[A]): Int = foldLeft(l, 0)((index, _ ) => index + 1)

  def revers[A](l: List2[A]): List2[A] = foldLeft(l, List2[A]())((ll: List2[A], l: A) => Cons2(l, ll))

  def append[A](l: List2[A], x: A): List2[A] = foldRight(l, List2(x))((x: A, l: List2[A]) => Cons2(x, l))
  def append2[A](l: List2[A], x: List2[A]): List2[A] = foldRight(l, x)((x: A, l: List2[A]) => Cons2(x, l))

  def merge[A](l: List2[List2[A]]): List2[A] = foldRight(l, List2[A]())((il:List2[A], nl: List2[A]) => foldRight(il, nl)((x: A, l: List2[A]) => Cons2(x, l)))

  def map[A, B](l: List2[A], f: A => B): List2[B] = foldRight(l, List2[B]())((a, b) => (Cons2(f(a),b)))

  def filter[A](l: List2[A], f: A => Boolean): List2[A] = foldRight(l, List2[A]())((a, b) => f(a) match {
    case true => Cons2(a,b)
    case false => b
  })

  def flatMap[A, B](as: List2[A])(f: A => List2[B]): List2[B] = merge(map(as, f))

  def filter2[A](as: List2[A], f: A => Boolean): List2[A] = flatMap(as)(a => f(a) match {
    case true => Cons2(a, Nil2)
    case false => Nil2
  })

  def zipWith[A](as: List2[A], ax: List2[A])(f: (A,A) => A): List2[A] = {
    def loop(as: List2[A], ax: List2[A], r: List2[A]): List2[A] = as match{
      case Nil2 => r
      case Cons2(x, xs) => {
        ax match {
          case Cons2(y, ys) => loop(xs, ys, append(r, f(x,y)))
        }
      }
    }
    loop(as, ax, List2[A]())
  }

  def head[A](as: List2[A]): A = as match {
    case Cons2(x, xs) => x
  }

  def hasSubsequence[A](as: List2[A], sub: List2[A]): Boolean = {
    def loop(o: List2[A], s: List2[A], r: List2[A]): Boolean = {
      if(sub != r) {
        o match {
          case Nil2 => false
          case Cons2(x, xs) => if(x == head(s)) {
            loop(xs, tail(s), append(r, x))
          }  else {
            loop(xs, sub, List2[A]())
          }
        }
      } else {
        true
      }
    }
    loop(as, sub, List2[A]())
  }

}

class Chapter3Test extends FunSpec with Matchers {
  import List2._

  it("Tail works") {
    tail(List2(1,2,3,4)) should be(List2(2,3,4))
    tail(List2()) should be(Nil2)
  }

  it("setHead works") {
    setHead(List2(1,2,3,4), 5) should be(List2(5,2,3,4))
    setHead(List2(), 5) should be(List2(5))
  }

  it("Drop works") {
    drop(List2(1,2,3),1) should be(List2(2,3))
    drop(List2(1,2,3),2) should be(List2(3))
    drop(List2(1,2,3),3) should be(List2())
    drop(List2(1,2,3),4) should be(Nil2)

  }

  it("Drop while") {
    dropWhile(List2(1,2,3,4), (x: Int) => x == 2) should be(List2(2,3,4))
    dropWhile(List2(1,2,3,4), (x: Int) => x == 5) should be(List2())
  }

  it("Init") {
    init(List2(1,2,3,4)) should be(List2(1,2,3))
  }

  it("Test fold") {
    foldRight(List2(1,2,3,4), Nil2:List2[Int])(Cons2(_,_)) should be(List2(1,2,3,4))
  }

  it("Length") {
    lengths(List2()) should be(0)
    lengths(List2(1,2,3)) should be(3)
  }

  it("fold left") {
    foldLeft(List2(1,2,3,4), 0)(_ + _) should equal(10)
  }

  it("sum 3 ") {
    sum3(List2(1,2,3,4)) should equal(10)
  }

  it("lengths 2 ") {
    lengths2(List2(1,2,3,4)) should equal(4)
  }

  it("reverse") {
    revers(List2(1,2,3)) should be(List2(3,2,1))
  }

  it("append") {
    append(List2(1,2), 3) should be(List2(1,2,3))
  }

  it("Merge") {
    merge(List2(List2(1,2), List2(3,4,5))) should be(List2(1,2,3,4,5))
  }

  it("Map") {
    map( List2(1,2,3), (x :Int) => { 1 + x }) should be(List2(2,3,4))
    map(List2("a", "b", "c"), (x: String) => {s"$x x"}) should be(List2("a x", "b x", "c x"))
  }

  it("Filter") {
    filter(List2(1,2,3,4), (x: Int) => x % 2 == 0) should be(List2(2,4))
  }

  it("flatMap") {
    flatMap(List2(1,2,3))(i => List2(i,i)) should be(List2(1,1,2,2,3,3))
  }

  it("Filter2") {
    filter2(List2(1,2,3,4), (x: Int) => x % 2 == 0) should be(List2(2,4))
  }

  it("zipWith") {
    zipWith(List2(1,2,3), List2(4,5,6))((a, b) => a + b) should be(List2(5,7,9))
  }

  it("subsequence") {
    hasSubsequence(List2(1,2,3,4), List2(2,3)) should be(true)
    hasSubsequence(List2(1,2,3,4), List2(2,4)) should be(false)
    hasSubsequence(List2(1,2,3,4), List2(1)) should be(true)
    hasSubsequence(List2(1,2,3,4), List2()) should be(true)
    hasSubsequence(List2(1,2,3,4), List2(1,2,3,4,5)) should be(false)
  }
}
