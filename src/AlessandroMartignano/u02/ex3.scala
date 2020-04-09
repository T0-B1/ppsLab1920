package AlessandroMartignano.u02

object ex3 extends App {

  // a

  val parity: Int => String = {
    case n if n % 2 == 0 => "even"
    case _ => "odd"
  }

  def parityMeth(x: Int): String = x match {
    case n if n % 2 == 0 => "even"
    case _ => "odd"
  }

  println("a")
  println(parity(0))
  println(parity(1))
  println(parity(2))
  println(parityMeth(0))
  println(parityMeth(1))
  println(parityMeth(2))

  //b

  def negMeth(pred: String => Boolean): String => Boolean =
    !pred(_)

  val neg: (String => Boolean) => (String => Boolean) = {
    case pred => !pred(_)
  }

  val empty: String => Boolean = _=="" // predicate on strings
  val notEmpty = neg(empty) // which type of notEmpty?
  println("b")
  println(notEmpty("foo"))// true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  //c

  def genNeg [A] (pred: A => Boolean): A => Boolean =
    !pred(_)

  val positive: Int => Boolean = n => n >= 0
  val emptyStr: String => Boolean = s => s == ""

  println("c")
  println(genNeg(positive)(2)) //false
  println(genNeg(emptyStr)("foo")) //true
}
