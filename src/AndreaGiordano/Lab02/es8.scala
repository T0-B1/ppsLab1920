package AndreaGiordano.Lab02

object es8 extends App {
  sealed trait Option[A] // An Optional data type
  object Option {
    case class None[A]() extends Option [A]
    case class Some[A](a:A) extends Option [A]

    def isEmpty [A](opt:Option[A]): Boolean = opt match {
      case None() => true
      case _ => false
    }

    def getOrElse [A, B >: A](opt:Option[A], orElse:B):B = opt match {
      case Some(a) => a
      case _ => orElse
    }

    def flatMap [A,B](opt:Option[A])(f:A => Option[B]):Option[B] = opt match {
      case Some(a) => f(a)
      case _ => None()
    }

    def filter [A](opt:Option[A])(pred:A=>Boolean):Option[A] = opt match {
      case Some(a) if pred(a) => Some(a)
      case _ => None()
    }

    def map[A,B](opt:Option[A])(f:A=>B):Option[B] = opt match {
      case Some(a) => Some(f(a))
      case _ => None()
    }

    def map2[A,B,C](opt1:Option[A],opt2:Option[B])(f:(A,B)=>C):Option[C] = (opt1,opt2) match {
      case (Some(a),Some(b)) => Some(f(a,b))
      case _ => None()
    }
  }

  import Option._
  val o1:Option[Int] = Some(1)
  val o2:Option[String] = Some("Ciao")
  val o3:Option[Boolean] = Some(true)
  val o4:Option[Int] = Some(3)
  val o5:Option[Double] = None()

  val test = map2(filter(map(o1)(_+22))(_=>getOrElse(o3,false)),o2) ((x,y)=>{println(y);x*1.1})
  println(test)
}
