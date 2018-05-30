package chapter3

import chapter3.List._
import org.scalatest._

class chapter3spec extends FunSpec with Matchers {
  describe("list") {
    describe("tail") {
      describe("list of int") {
        it("should return tail of list") {
          tail(chapter3.List(1, 2, 3)) shouldEqual chapter3.List(2, 3)
        }
      }

      describe("list of string") {
        it("should return tail of list") {
          tail(chapter3.List("a", "b", "c")) shouldEqual chapter3.List("b", "c")
        }
      }

      describe("empty list") {
        it("should return empty list") {
          tail(chapter3.Nil) shouldEqual chapter3.Nil
        }
      }
    }

    describe("setHead") {
      describe("list of int") {
        it("should return tail of list") {
          setHead(chapter3.List(1, 2, 3), 4) shouldEqual chapter3.List(4, 2, 3)
        }
      }

      describe("list of string") {
        it("should return tail of list") {
          setHead(chapter3.List("a", "b", "c"), "d") shouldEqual chapter3.List("d", "b", "c")
        }
      }

      describe("empty list") {
        it("should return empty list") {
          setHead(chapter3.Nil, 1) shouldEqual chapter3.Nil
        }
      }
    }

    describe("drop") {
      describe("list of int") {
        describe("drop 1 element") {
          it("should return list with first element removed") {
            drop(chapter3.List(1, 2, 3), 1) shouldEqual chapter3.List(2, 3)
          }
        }

        describe("drop 2 elements") {
          it("should return list with first two elements removed") {
            drop(chapter3.List(1, 2, 3, 4), 2) shouldEqual chapter3.List(3, 4)
          }
        }
      }
    }

    describe("dropWhile") {
      describe("list of int") {
        describe("drop all elements < 3") {
          it("should return list with first two elements revmoed") {
            val f = (x: Int) => x < 3
            dropWhile(chapter3.List(1, 2, 3, 4), f) shouldEqual chapter3.List(3, 4)
          }
        }
      }

      describe("list of string") {
        describe("drop all elements before C") {
          it("should return list with first two elements removed") {
            val f = (x: String) => x != "c"
            dropWhile(chapter3.List("a", "b", "c", "d"), f) shouldEqual chapter3.List("c", "d")
          }
        }
      }
    }

    describe("init") {
      describe("list of int") {
        it("should return tail of list") {
          init(chapter3.List(1, 2, 3)) shouldEqual chapter3.List(1, 2)
        }
      }

      describe("list of string") {
        it("should return tail of list") {
          init(chapter3.List("a", "b", "c")) shouldEqual chapter3.List("a", "b")
        }
      }

      describe("empty list") {
        it("should return empty list") {
          init(chapter3.Nil) shouldEqual chapter3.Nil
        }
      }
    }

    describe("length") {
      describe("empty list") {
        it("should return 0") {
          chapter3.List.length(chapter3.Nil) shouldEqual 0
        }
      }

      describe("non-empty list") {
        it("should return 3") {
          chapter3.List.length(chapter3.List("a", "b", "c")) shouldEqual 3
        }
      }
    }

    describe("foldLeft") {
      describe("list of int - sum") {
        it("should return 6") {
          foldLeft(chapter3.List(1, 2, 3), 0)((e, acc) => acc + e) shouldEqual 6
        }
      }

      describe("list of string - concat") {
        it("should return abc") {
          foldLeft(chapter3.List("a", "b", "c"), "")((e, acc) => acc + e) shouldEqual "abc"
        }
      }
    }
  }
}