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
}