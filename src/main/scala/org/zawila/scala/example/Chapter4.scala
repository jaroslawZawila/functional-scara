package org.zawila.scala.example

import org.scalatest.{FunSpec, Matchers}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =  flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

class Chapter4 extends FunSpec with Matchers {

  it("map") {
    Some(1).map(x => x * 5) should be(Some(5))
    None.map(_ => ()) should be(None)
  }

  it("flatMap") {
    Some(1).flatMap(x => Some(x.toString)) should be(Some("1"))
    Some(1).flatMap(_ => None) should be(None)
  }

  it("getOrElse") {
    Some(1).getOrElse(3) should be(1)
    None.getOrElse(3) should be(3)
  }

  it("orElse") {
    Some(1).orElse(None) should be(Some(1))
    None.orElse(Some(1)) should be(Some(1))
  }

  it("filter") {
    Some(1).filter(x => x > 0) should be(Some(1))
    Some(1).filter(x => x < 0) should be(None)
  }
}
