package org.sbttest

object FuntionalScala {
  private def abs(n: Int) = {
    if (n >= 0)
      n
    else
      -n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def factorial(n: Int) = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = {
      if(n == 1)
        acc
      else
        loop(n - 1, acc * n)
    }

    loop(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f:Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  private def fib(n: Int) = {
    @annotation.tailrec
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n == 0)
        a
      else
        loop(b, a + b, n - 1)
    }

    loop(1, 1, n)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n == as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n == 0 || n == as.length)
        true
      else if (ordered(as(n - 1), as(n)))
        loop(n + 1)
      else
        false

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) =>
      (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) =>
      f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) =>
      f(g(a))

  def main(args: Array[String]): Unit = {
    val n = 5

    println(formatResult("factorial", n, factorial))
    println(formatResult("fibonacci", n, fib))

    println(findFirst(Array(7, 9, 13), (x: Int) => x == 9))
    println(isSorted(Array(7, 9, 13), (x: Int, y: Int) => (x <= y)))
  }
}
