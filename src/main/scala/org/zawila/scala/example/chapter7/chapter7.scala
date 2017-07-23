package org.zawila.scala.example.chapter7

import java.util.concurrent.ExecutorService

import org.zawila.scala.example.chapter7.chapter7.Par

import scala.concurrent.Future

object chapter7 {

  type Par[A] = ExecutorService => Future[A]


  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}

// 7.3

//class Par[A] {
//
//}


object Par {

  def unit[A](a: A): Par[A] = ???
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
  def fork[A](a: => Par[A]): Par[A] = ???
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = s(a)

  def map2[A, B, C](f1: Par[A], f2: Par[B])(f: (A, B) => C): C = ???

  def fork[A](a: => Par[A]): Par[A] = ???
}

