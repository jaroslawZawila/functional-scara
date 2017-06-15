package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = n match {
    case 0 => 0
    case _ => {
      @tailrec
      def go(index: Int, prev: Int, sum: Int): Int = {
        if(index == 0){
          sum
        } else {
          go(index - 1, sum, sum + prev)
        }
      }
      go(n, 1, 0)
    }
  }
}

class FibonacciTest extends FunSpec with Matchers {

  import Fibonacci._

  it("Result for 0 should be 0") {
    fib(0) should equal(0)
  }

  it("Result for 1 should be 1") {
    fib(1) should equal(1)
  }

  it("Result for 2 should be 1") {
    fib(2) should equal(1)
  }

  it("Result for 3 should be 2") {
    fib(3) should equal(2)
  }

  it("Result for 4 should be 3") {
    fib(4) should equal(3)
  }

  it("Result for 5 should be 5") {
    fib(5) should equal(5)
  }

  it("Result for 6 should be 8") {
    fib(6) should equal(8)
  }
}
