package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

trait RNG {
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(SimpleRNG.nonNegativeInt)(i => i - i % 2)

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def double2(rng: RNG): Rand[Double] = map(SimpleRNG.nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)
  }

  //6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  //6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  //6.9 flat map implementation
  def map3[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

object SimpleRNG {
  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, r) if n > 0 && n <= Int.MaxValue => (n,r)
    case (_, r) => nonNegativeInt(r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, _) = rng.nextInt
    val (d, r) = SimpleRNG.double(rng)
    ((n,d), r)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((n,d),r) => ((d,n), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(m: Int,rng: RNG, l: List[Int]): (List[Int], RNG) = {
      if(m > 0) {
        val (n, r) = rng.nextInt
        loop(m -1, r, n :: l)
      } else {
        (l,rng)
      }
    }
    loop(count, rng, List.empty[Int])
  }

}

class RNGTest extends FunSpec with Matchers {

  it("nonNegativeInt") {
    val rng = SimpleRNG(1)
    SimpleRNG.nonNegativeInt(rng) should be(384748,SimpleRNG(25214903928l))
  }

  it("double") {
    val rng = SimpleRNG(10)
    SimpleRNG.double(rng) should be(0.0017916266814766576,SimpleRNG(252149039181l))
  }

  it("intDouble") {
    val rng = SimpleRNG(10)
    SimpleRNG.intDouble(rng) should be((3847489,0.0017916266814766576),SimpleRNG(252149039181l))
  }

  it("doubleInt") {
    val rng = SimpleRNG(10)
    SimpleRNG.doubleInt(rng) should be((0.0017916266814766576, 3847489),SimpleRNG(252149039181l))
  }

  it("double 3") {
    val rng = SimpleRNG(10)
    SimpleRNG.double3(rng) should be(((0.0017916266814766576,0.6213264384406276,0.6923740779479844),SimpleRNG(97442988689487l)))
  }

  it("Random list") {
    val rng = SimpleRNG(10)
    SimpleRNG.ints(5)(rng) should be((List(-1453296530, 711662464, 1486862010, 1334288366, 3847489),SimpleRNG(186231735346465l)))
    SimpleRNG.ints(-5)(rng) should be((List(),SimpleRNG(10)))
  }

  it("double2") {
    val rng = SimpleRNG(10)
    val x = rng.double2(rng)
    x(rng) should be(0.0017916266814766576,SimpleRNG(252149039181l))

  }


}