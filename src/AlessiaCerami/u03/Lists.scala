package AlessiaCerami.u03
object Lists {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def appends[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, appends(t, l2))
      case _ => l2
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    }

    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }

    def drop[A](l: List[A], n: Int): List[A] = l match{
      case Cons(_,t) if n == 1 => t
      case Cons(h,t) if n <= 0 => Cons(h,t)
      case Cons(_,t) => drop( t, n-1 )
      case Nil() => Nil()
    }

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match{
      case Cons(h,t) => appends(f(h),flatMap(t)(f))
      case Nil() => Nil()
    }

    def map2[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(x => Cons(mapper(x),Nil()))

    def filter2[A](l: List[A])(pred: A=>Boolean): List[A] = flatMap(l)(x => if(pred(x)) Cons(x,Nil()) else Nil())

    def max(l: List[Int]):Option[Int] = {
      def _max(maxCurr:Int, list:List[Int]): Option[Int] = list match{
        case Cons(h,t)  if maxCurr > h => _max(maxCurr, t)
        case Cons(h,t)  if h > maxCurr => _max(h, t)
        case Nil() if maxCurr == 0 => None
        case Nil() => Some(maxCurr)
      }
      _max(0, l)
    }

    def leftFold[A,B](lst: List[A])(acc:B)(f:(B,A) =>B):B = lst match {
      case Cons(h,t) => leftFold(t)(f(acc,h))(f)
      case Nil() => acc
    }

    def rightFold[A,B](lst: List[A])(acc:B)(f:(A,B) =>B):B = lst match {
      case Cons(h,t) => f(h, rightFold(t)(acc)(f))
      case Nil() => acc
    }
  }
}

object ListsMain extends App {
  import Lists._
  import List._
  import AlessiaCerami.u03.Lists.List

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  val lst = Cons(20,Cons(30,Nil()))
  val emptyList:List[Int] = Nil()
  val list = Cons(3,Cons(7,Cons(1,Cons(5,Nil()))))

  println(List.sum(l)) // 60
  println(appends(Cons(5, Nil()), l)) // 5,10,20,30
  println(filter[Int](l)(_ >=20)) // 20,30

  println(drop(lst,1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop(lst,2)) // Cons (30 , Nil ())
  println(drop(lst,5)) // Nil ()
  println("ZERO--> "+drop(lst,0), "NEGATIVE--> "+drop(lst,-1)) // Nil ()

  println(flatMap(lst)(v => Cons(v + 1,Nil()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(flatMap(lst)(v => Cons(v + 1,Cons(v + 2 ,Nil()))))  // Cons(12,Cons(21,Cons(22,Cons(31,Cons(32,Nil())))))

  //leftFold
  println(max(Cons(10,Cons(25,Cons(20,Nil()))))) // Some (25)
  println(max(Nil())) // None()

  println("-------------LEFTFOLD---------------")
  println(list)
  println(leftFold(list)(0)(_-_)) // -16
  println(leftFold(emptyList)(0)(_-_)) //
  println(rightFold(list)(0)(_-_))
}