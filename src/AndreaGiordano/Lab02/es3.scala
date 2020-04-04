package AndreaGiordano.Lab02

object es3 extends App {
  val parity:(Int)=>String = (n)=> if(n % 2 == 0) "even" else "odd"

  println(parity(6),parity(5))

  def parityWithDef(n:Int):String = if(n % 2 == 0) "even" else "odd"

  println(parityWithDef(6),parityWithDef(5))

  def negD(f:String=>Boolean): String=>Boolean = !f(_)
  val neg:(String=>Boolean) => String=>Boolean = (f) => !f(_)

  val empty: String => Boolean = _=="" // predicate on strings
  val notEmpty = neg(empty) // which type of notEmpty?
  val notEmptyD = negD(empty) // which type of notEmpty?
  println(notEmpty("foo"),notEmpty(""))// true notEmpty("") // false
  println(notEmptyD("foo"),notEmptyD(""))// true notEmpty("") // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  def negG[A](f:A=>Boolean):A=>Boolean = !f(_)
  val zero: Int=>Boolean = _==0
  val notZero = negG(zero)
  val notTrue: Boolean=>Boolean = (x)=> !x
  val siTrue = negG(notTrue)
  println(notZero(0),notZero(1))
  println(siTrue(true),siTrue(false))

}
