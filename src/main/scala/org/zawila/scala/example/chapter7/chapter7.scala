package org.zawila.scala.example.chapter7

object chapter7 {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

}

// 7.1

class Par[A] {

}

object Par {
  def unit[A](a: A) = ???

  def map2[A, B, C](f1: Par[A], f2: Par[B])(f: (A, B) => C) = ???
}

