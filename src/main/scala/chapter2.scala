object chapter2 {
  // Exercise 2.1
  // calculate nth Fibonacci number using a local an annotated tail-recursive function

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, rem: Int): Int = {
      if (rem == 0) b else go(b, a + b, rem - 1)
    }

    n match {
      case 0 => 0
      case x if x < 3 => 1
      case _ => go(1, 2, n - 3)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.zip(as.drop(1)).forall{case (left, right) => ordered(left, right)}
  }
}
