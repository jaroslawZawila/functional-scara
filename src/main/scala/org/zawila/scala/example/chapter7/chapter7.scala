package org.zawila.scala.example.chapter7

object chapter7 {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

}

object  Par {
  def unit[A](a: A) = ???
  def map2[A, B, C](f1: A => B, f2: A => B)(f: (B,B) => C) = ???
}

