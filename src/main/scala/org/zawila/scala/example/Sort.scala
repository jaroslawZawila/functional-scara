package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

object Sort {
  def isSorted[A](array: Array[A], ordered: (A,A) => Boolean) = array.length match {
    case 0 => true
    case 1 => true
    case _ => {
      @tailrec
      def loop(index: Int): Boolean = {
        val v1 = array(index - 1)
        val v2 = array(index)
        ordered(v1, v2) match {
          case true => if (index == array.length - 1) true else loop(index + 1)
          case false => false
        }
      }
      loop(1)
    }
  }
}

class SortTest extends FunSpec with Matchers {
  import Sort._

  def naiveSort(a: Int, b: Int) = a < b

  it("Sort it returns true for sorted array") {
    val array = Array(1,2,3)
    isSorted(array, naiveSort) should be(true)
  }

  it("Sort it returns false for not sorted array") {
    val array = Array(1,5,3)
    isSorted(array, naiveSort) should be(false)
  }

  it("Sort it returns true for empty array") {
    val array = Array.empty[Int]
    isSorted(array, naiveSort) should be(true)
  }

  it("Sort it returns true for one element array") {
    val array = Array(1)
    isSorted(array, naiveSort) should be(true)
  }

}
