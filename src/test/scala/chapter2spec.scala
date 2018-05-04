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
      describe("int") {
        val f: (Int, Int) => Boolean = _ <= _

        it("should return true for ascending ordered array") {
          isSorted(Array(1, 2, 3), f) shouldBe true
        }

        it("should return false for unordered array") {
          isSorted(Array(1, 3, 2), f) shouldBe false
        }
      }

      describe("double") {
        val f: (Double, Double) => Boolean = _ <= _

        it("should return true for ascending ordered array") {
          isSorted(Array(1.1, 2.2, 3.3), f) shouldBe true
        }

        it("should return false for unordered array") {
          isSorted(Array(1.1, 3,3, 2.2), f) shouldBe false
        }
      }

      describe("string") {
        val f: (String, String) => Boolean = _ <= _

        it("should return true for ascending ordered array") {
          isSorted(Array("abc", "def", "ghi"), f) shouldBe true
        }

        it("should return false for unordered array") {
          isSorted(Array("abc", "ghi", "def"), f) shouldBe false
        }
      }
    }

    describe("by greater than int") {
      val f: (Int, Int) => Boolean = _ >= _

      it("should return true for descending ordered array") {
        isSorted(Array(3, 2, 1), f) shouldBe true
      }

      it("should return false for unordered array") {
        isSorted(Array(3, 1, 2), f) shouldBe false
      }
    }

    describe("by less than lower cased string") {
      val f: (String, String) => Boolean = _.toLowerCase <= _.toLowerCase

      it("should return true for ascending ordered array") {
        isSorted(Array("AbC", "dEf", "GhI"), f) shouldBe true
      }

      it("should return false for unordered array") {
        isSorted(Array("AbC", "GhI", "dEf"), f) shouldBe false
      }
    }
  }

  describe("curry") {
    def f(x: Int, y: Int) = x * y
    val curried = curry(f)
    val partial = curried(3)

    it("should return product of argument and 3") {
      partial(3) shouldEqual 9
      partial(7) shouldEqual 21
    }
  }

  describe("uncurry") {
    def f(x: Int)(y: Int) = x * y
    val uncurried = uncurry(f)

    it("should return product of arguments") {
      uncurried(3, 3) shouldEqual 9
      uncurried(3, 7) shouldEqual 21
    }
  }
}