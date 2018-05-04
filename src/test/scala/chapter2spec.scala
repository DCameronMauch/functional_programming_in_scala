import chapter2._
import org.scalatest._

class chapter2spec extends FunSpec with Matchers{
  describe("fib") {
    it("should return correct values") {
      fib(0) shouldEqual 0
      fib(1) shouldEqual 1
      fib(2) shouldEqual 1
      fib(3) shouldEqual 2
      fib(4) shouldEqual 3
      fib(5) shouldEqual 5
      fib(6) shouldEqual 8
      fib(7) shouldEqual 13
      fib(8) shouldEqual 21
      fib(9) shouldEqual 34
    }
  }

  describe("isSorted") {
    describe("by less than") {
      describe("int list") {
        val f: (Int, Int) => Boolean = _ <= _

        it("should return true for ascending ordered lists") {
          isSorted(Array(1, 2, 3), f) shouldBe true
          isSorted(Array(1, 3, 2), f) shouldBe false
        }
      }

      describe("double list") {
        val f: (Double, Double) => Boolean = _ <= _

        it("should return true for ascending ordered lists") {
          isSorted(Array(1.1, 2.2, 3.3), f) shouldBe true
          isSorted(Array(1.1, 3,3, 2.2), f) shouldBe false
        }
      }

      describe("string list") {
        val f: (String, String) => Boolean = _ <= _

        it("should return true for ascending ordered lists") {
          isSorted(Array("abc", "def", "ghi"), f) shouldBe true
          isSorted(Array("abc", "ghi", "def"), f) shouldBe false
        }
      }
    }

    describe("by greater than") {
      describe("int list") {
        val f: (Int, Int) => Boolean = _ >= _

        it("should return true for ascending ordered lists") {
          isSorted(Array(3, 2, 1), f) shouldBe true
          isSorted(Array(3, 1, 2), f) shouldBe false
        }
      }

    }
  }
}