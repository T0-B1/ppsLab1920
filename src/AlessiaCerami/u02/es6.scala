package AlessiaCerami.u02

object es6 extends App {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def _fib (n: Int, prev: Int, current:Int): Int = n match {
      case 0 => prev
      case n => _fib(n-1, current, prev + current)
    }
    _fib(n, 0, 1)
  }
  println(fib(0),fib(1),fib(2),fib(3),fib(4),fib(5),fib(6),fib(7),fib(8),fib(9),fib(10))

  //0,1,1,2,3,5,8,13,21,34,55
}
