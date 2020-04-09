package AlessandroMartignano.u02

object ex4 extends App {

  /**
   *
  Implement a predicate that checks whether its arguments x; y; z respect
    the relation x <= y <= z, in 4 variants (curried/non-curried x val/def)
  val p1: <CurriedFunType> = ...
  val p2: <NonCurriedFunType> = ...
    def p3(...)(...)(...): ... = ...
    def p4(...): ... = ...
    Notice: function types and function literals are syntactically similar

  */

  val p1: Int => Int => Int => Boolean = a => b => c => a <= b && b <= c
  val p2: (Int, Int, Int) => Boolean = (a, b, c) => a <= b && b <= c
  def p3(a: Int)(b: Int)(c: Int): Boolean = a <= b && b <= c
  def p4(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

  println(p1(3)(4)(4))
  println(p1(3)(4)(3))
  println(p2(3,4,4))
  println(p2(3,4,3))
  println(p3(3)(4)(4))
  println(p3(3)(4)(3))
  println(p4(3,4,4))
  println(p4(3,4,3))
}
