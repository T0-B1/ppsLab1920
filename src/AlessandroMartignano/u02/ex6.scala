package AlessandroMartignano.u02

object ex6 extends App {

  /**
   * Create a function to get the n-th Fibonacci number
   * I The first two Fibonacci numbers are 0 and 1. The following Fibonacci
   * numbers are given by the sum of their previous two.
   * I Signature: fib(n: Int): Int
   * I Example: (fib(0),fib(1),fib(2),fib(3),fib(4)) // (0,1,1,2,3)
   * I Hint: look at factorial in lecture 02
   * I Question: is your recursion a tail one? How could you be sure?
   */

  def fibo(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fibo(n-1) + fibo(n-2)
  }

  println(fibo(4))
  println(fibo(5))

  def fiboTR(n: Int): Int = {
    @annotation.tailrec  // checks only if optimisation is possible
    def _fibo(a: Int, b: Int, n: Int): Int = n match {
      case 0 => a
      case _ => _fibo(b, a+b, n-1)
  }
    _fibo(0, 1, n)
  }

  println(fiboTR(6))
  println(fiboTR(7))
  println(fiboTR(8))
}
