package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

object Excersices2 {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (x: A) => (z: B) => f(x, z)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (x: A, y: B) => f(x)(y)

  def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))
}

class Excersices2Test extends FunSpec with Matchers {
  import Excersices2._

  it("Carry function works") {
    def test(x: Int, y: Double): String = s"$x : $y"

    curry(test)(1)(2.5) should equal("1 : 2.5")
  }

  it("Uncarry function works") {
    def test(x: Int)(y: Int): Int = x * y

    uncurry(test)(2,5) should be(10)
  }

  it("Compose work fine") {
    def f1(x: Int): String = s"x: $x"
    def f2(y: String): String = s"y: $y"

    compose(f2, f1)(5) should equal("y: x: 5")
  }
}