package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] =  l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](list: List[A], first: A) = list match {
    case Nil => List(first)
    case Cons(x, t) => Cons(first, t)
  }

  def drop[A](l: List[A], idx: Int): List[A] = {
    def loop(l: List[A], index:Int): List[A] = index match {
      case x if x < idx => loop(tail(l), index + 1)
      case _ => l
    }
    loop(l, 0)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) l else dropWhile(tail(l), f)
  }

  def init[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x,xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)(_ + _)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  def lengths[A](l: List[A]): Int = foldRight(l, 0)((_, c) => c + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def lengths2[A](l: List[A]): Int = foldLeft(l, 0)((index, _ ) => index + 1)

  def revers[A](l: List[A]): List[A] = foldLeft(l, List[A]())((ll: List[A], l: A) => Cons(l, ll))

  def append[A](l: List[A], x: A): List[A] = foldRight(l, List(x))((x: A, l: List[A]) => Cons(x, l))
  def append2[A](l: List[A], x: List[A]): List[A] = foldRight(l, x)((x: A, l: List[A]) => Cons(x, l))

  def merge[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(( il:List[A], nl: List[A]) => foldRight(il, nl)((x: A, l: List[A]) => Cons(x, l)))

  def map[A, B](l: List[A], f: A => B): List[B] = foldRight(l, List[B]())((a,b) => (Cons(f(a),b)))

  def filter[A](l: List[A], f: A => Boolean): List[A] = foldRight(l, List[A]())((a,b) => f(a) match {
    case true => Cons(a,b)
    case false => b
  })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = merge(map(as, f))

  def filter2[A](as: List[A], f: A => Boolean): List[A] = flatMap(as)(a => f(a) match {
    case true => Cons(a, Nil)
    case false => Nil
  })

  def zipWith[A](as: List[A], ax: List[A])(f: (A,A) => A): List[A] = {
    def loop(as: List[A], ax: List[A], r: List[A]): List[A] = as match{
      case Nil => r
      case Cons(x, xs) => {
        ax match {
          case Cons(y, ys) => loop(xs, ys, append(r, f(x,y)))
        }
      }
    }
    loop(as, ax, List[A]())
  }

  def head[A](as: List[A]): A = as match {
    case Cons(x, xs) => x
  }

  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    def loop(o: List[A],s: List[A], r: List[A]): Boolean = {
      if(sub != r) {
        o match {
          case Nil => false
          case Cons(x, xs) => if(x == head(s)) {
            loop(xs, tail(s), append(r, x))
          }  else {
            loop(xs, sub, List[A]())
          }
        }
      } else {
        true
      }
    }
    loop(as, sub, List[A]())
  }

}

class Chapter3Test extends FunSpec with Matchers {
  import List._

  it("Tail works") {
    tail(List(1,2,3,4)) should be(List(2,3,4))
    tail(List()) should be(Nil)
  }

  it("setHead works") {
    setHead(List(1,2,3,4), 5) should be(List(5,2,3,4))
    setHead(List(), 5) should be(List(5))
  }

  it("Drop works") {
    drop(List(1,2,3),1) should be(List(2,3))
    drop(List(1,2,3),2) should be(List(3))
    drop(List(1,2,3),3) should be(List())
    drop(List(1,2,3),4) should be(Nil)

  }

  it("Drop while") {
    dropWhile(List(1,2,3,4), (x: Int) => x == 2) should be(List(2,3,4))
    dropWhile(List(1,2,3,4), (x: Int) => x == 5) should be(List())
  }

  it("Init") {
    init(List(1,2,3,4)) should be(List(1,2,3))
  }

  it("Test fold") {
    foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3,4))
  }

  it("Length") {
    lengths(List()) should be(0)
    lengths(List(1,2,3)) should be(3)
  }

  it("fold left") {
    foldLeft(List(1,2,3,4), 0)(_ + _) should equal(10)
  }

  it("sum 3 ") {
    sum3(List(1,2,3,4)) should equal(10)
  }

  it("lengths 2 ") {
    lengths2(List(1,2,3,4)) should equal(4)
  }

  it("reverse") {
    revers(List(1,2,3)) should be(List(3,2,1))
  }

  it("append") {
    append(List(1,2), 3) should be(List(1,2,3))
  }

  it("Merge") {
    merge(List(List(1,2), List(3,4,5))) should be(List(1,2,3,4,5))
  }

  it("Map") {
    map( List(1,2,3), (x :Int) => { 1 + x }) should be(List(2,3,4))
    map(List("a", "b", "c"), (x: String) => {s"$x x"}) should be(List("a x", "b x", "c x"))
  }

  it("Filter") {
    filter(List(1,2,3,4), (x: Int) => x % 2 == 0) should be(List(2,4))
  }

  it("flatMap") {
    flatMap(List(1,2,3))(i => List(i,i)) should be(List(1,1,2,2,3,3))
  }

  it("Filter2") {
    filter2(List(1,2,3,4), (x: Int) => x % 2 == 0) should be(List(2,4))
  }

  it("zipWith") {
    zipWith(List(1,2,3), List(4,5,6))((a, b) => a + b) should be(List(5,7,9))
  }

  it("subsequence") {
    hasSubsequence(List(1,2,3,4), List(2,3)) should be(true)
    hasSubsequence(List(1,2,3,4), List(2,4)) should be(false)
    hasSubsequence(List(1,2,3,4), List(1)) should be(true)
  }
}
