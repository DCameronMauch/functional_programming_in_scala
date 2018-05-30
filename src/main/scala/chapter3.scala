package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
   }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2)) }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f)) }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x ,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 2
  // write a tail function that returns the list without the first element

  def tail[A](list: List[A]): List[A] = ???

  // Exercise 3
  // write a setHead function to replace the head of a list with a given element

  def setHead[A](list: List[A], element: A): List[A] = ???

  // Exercise 4
  // write a drop function to remove the first N elements from a list

  def drop[A](list: List[A], n: Int) : List[A] = ???

  // Exercise 5
  // write a dropWhile function to remove leading elements from a list
  // while a provided function evaluates to true

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 6
  // write a init function to return list with last element removed

  def init[A](l: List[A]): List[A] = ???

  // Exercise 9
  // write a length function that using foldRight

  def length[A](as: List[A]): Int = ???

  // Exercise 10
  // write a foldLeft function that uses tail recursion

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???

  // Exercise 11
  // write a sum3 function to add together list of int using foldLeft
  // write a product3 function to multiply together list of double using foldLeft
  // write a length2 function using FoldLeft

  def sum3(ints: List[Int]): Int = ???

  def product3(ds: List[Double]): Double = ???

  def length2[A](as: List[A]): Int = ???
}