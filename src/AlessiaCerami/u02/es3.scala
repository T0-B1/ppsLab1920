package AlessiaCerami.u02

object es3 extends App {

  // ---------------------------------------- A: parity ----------------------------------------
  val parity: (Int) => String = {
    case n if n % 2 == 0 => "even"
    case _ => "odd"
  }

  val parityIfElse: Int => String = (x) => if (x % 2 == 0) "even" else "odd"

  def parityWithMatch(x:Int):String = x match{
    case n if n % 2 == 0 => "even"
    case _ => "odd"
  }

  def parityWithDef(x:Int):String = if (x % 2 == 0) "even" else "odd"

  //println(parity(2))
  println(parityWithMatch(5))
  println(parityIfElse(5))
  //println(parityWithDef(15))

  // ---------------------------------------- B: negation ----------------------------------------

  def negD(f:String=>Boolean):String => Boolean = !f(_)
  val neg: (String => Boolean) => (String => Boolean) = f => !f(_)

  val empty: String => Boolean = _=="" // predicate on strings
  val notEmpty = neg(empty) // which type of notEmpty?
  println( notEmpty("foo"), notEmpty("") )// ( true, false )
  println( notEmpty("foo") && !notEmpty("") )// true.. a comprehensive test
  println(neg(_=="pippo")("pippo"))

  // --------------------------------------- B: negGenerics ---------------------------------------

  def negDG[A](f: A => Boolean):A=>Boolean = !f(_)

  val e: String => Boolean = _=="" // predicate on strings
  val nE = negDG(empty) // which type of notEmpty?

  val b:Boolean => Boolean = _ == true
  val nB = negDG(b)

  println( nE("foo"), nE("") )// ( true, false )
  println( nB(true), nB(false) )// ( false, true )
}
