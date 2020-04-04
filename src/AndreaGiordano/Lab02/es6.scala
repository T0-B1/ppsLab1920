package AndreaGiordano.Lab02

object es6 extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def recFib(prev1:Int,prev2:Int,count:Int):Int = count match{
      case 0 => prev1
      case n => recFib(prev1+prev2,prev1,count-1)
    }
    recFib(0,1,n)
  }

  // println(fib(0),fib(1),fib(2),fib(3),fib(4),fib(5),fib(6),fib(7),fib(8),fib(9),fib(10))

  // (0,1,1,2,3,5,8,13,21,34,55)

}
