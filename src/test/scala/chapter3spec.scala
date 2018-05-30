package chapter3

import chapter3.List._
import org.scalatest._

class chapter3spec extends FunSpec with Matchers {
  describe("list") {
    describe("tail") {
      describe("list of int") {
        tail(chapter3.List(1, 2, 3)) shouldEqual chapter3.List(2,3)
      }
      describe("list of string") {
        tail(chapter3.List("a", "b", "c")) shouldEqual chapter3.List("b", "c")
      }
      describe("empty list") {
        tail(chapter3.Nil) shouldEqual chapter3.Nil
      }
    }

    describe("setHead") {
      describe("list of int") {
        setHead(chapter3.List(1, 2, 3), 4) shouldEqual chapter3.List(4, 2, 3)
      }
      describe("list of string") {
        setHead(chapter3.List("a", "b", "c"), "d") shouldEqual chapter3.List("d", "b", "c")
      }
      describe("empty list") {
        setHead(chapter3.Nil, 1) shouldEqual chapter3.Nil
      }
    }
  }
}