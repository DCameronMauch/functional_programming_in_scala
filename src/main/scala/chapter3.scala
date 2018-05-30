package chapter3

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
}