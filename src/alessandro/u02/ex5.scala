package alessandro.u02

object ex5 extends App {

  /**
   * Create a function that implements functional compositions
   * I Signature: compose(f: Int => Int, g: Int => Int): Int => Int
   * I Example: compose(_-1,_*2)(5) // 9
   * I Create a generic version of compose
   * What signature? Is there any constraint?
   */

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => f(g(x))

  println(compose(_-1,_*2)(5))

  def composeGen [A] (f: A => A, g: A => A): A => A = (x: A) => f(g(x))

  println(composeGen[Int](_-3,_*2)(5))

}
