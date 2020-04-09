package AlessandroMartignano.u02

object ex8 extends App {

  sealed trait Option[A] // An Optional data type
  object Option {
    case class None[A]() extends Option[A]
    case class Some[A](a: A) extends Option[A]

    def isEmpty[A](opt: Option[A]): Boolean = opt match {
      case None() => true
      case _ => false
    }

    def getOrElse[A, B >: A](opt: Option[A], orElse: B): B = opt match {
      case Some(a) => a
      case _ => orElse
    }

    def flatMap[A,B](opt: Option[A])(f:A => Option[B]): Option[B] = opt match {
      case Some(a) => f(a)
      case _ => None()
    }

    def filter[A](opt: Option[A])(p: A => Boolean): Option[A] = opt match {
      case Some(a) if p(a) => Some(a)
      case _ => None()
    }

    def map[A,B](opt: Option[A])(m: A => B): Option[B] = opt match {
      case Some(a) => Some(m(a))
      case _ => None()
    }

    def map2[A,B,C](op1: Option[A])(op2: Option[B])(m: (A, B) => C): Option[C] = (op1, op2) match {
      case (Some(a), Some(b)) => Some(m(a, b))
      case _ => None()
    }
  }

  import Option._
  val s1: Option[Int] = Some(1)
  val s2: Option[Int] = Some(2)
  val s3: Option[Int] = None()

  println(s1) // Some(1)
  println(getOrElse(s1,0), getOrElse(s3,0)) // 1,0
  println(flatMap(s1)(i => Some(i+1))) // Some(2)
  println(flatMap(s1)(i => flatMap(s2)(j => Some(i+j)))) // Some(3)
  println(flatMap(s1)(i => flatMap(s3)(j => Some(i+j)))) // None

  println(filter(Some(5))(_ > 2)) // Some(5)
  println(filter(Some(5))(_ > 8)) // None

  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(None[Int])(_ > 2)) // None

  val appendBoolInt: (Boolean, Int) => String = (f, n) => "" + f + " " + n

  println(appendBoolInt(false, 7))
  println(map2(Some(true))(Some(2))(appendBoolInt))
  println(map2(None[Boolean])(Some(2))(appendBoolInt))

}
