package AndreaGiordano.Lab03

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

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
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

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_,t) if n > 1 => drop(t,n-1)
      case Cons(_,t) if n == 1 => t
      case Nil() => Nil()
      case _ => l
    }

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(h,t) => append(f(h),flatMap(t)(f))
      case _ => Nil()
    }

    def map2[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)( v => Cons(mapper(v),Nil()) )

    def filter2[A](l1: List[A])(pred: A=>Boolean): List[A] = flatMap(l1)( v => if(pred(v)) Cons(v,Nil()) else Nil())

    def max(l: List[Int]):Option[Int] = {
      @annotation.tailrec
      def _max(l:List[Int], currentMax:Int):Option[Int] = l match {
        case Cons(h,t) => _max(t,{if(h>currentMax) h else currentMax})
        case _ => {if(currentMax==Int.MinValue) None else Some(currentMax)}
      }
      _max(l,Int.MinValue)
    }

    @annotation.tailrec
    def foldLeft[A,B](l:List[A])(acc:B)(f:(B,A)=>B):B = l match {
      case Cons(h,t) => foldLeft(t)(f(acc,h))(f)
      case _ => acc
    }

    def foldRight[A,B](l:List[A])(acc:B)(f:(A,B)=>B):B = l match {
      case Cons(h,t) => f(h,foldRight(t)(acc)(f))
      case _ => acc
    }
  }
}

object ListsMain extends App {
  import Lists._
  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60
  import List._
  import AndreaGiordano.Lab03.Lists.List
  println(append(Cons(5, Nil()), l)) // 5,10,20,30
  println(filter[Int](l)(_ >=20)) // 20,30

  val lst = Cons(10, Cons(20, Cons(30, Cons(40, Cons(15, Cons(25, Nil()))))))

  println(drop(lst,1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop(lst,2)) // Cons (30 , Nil ())
  println(drop(lst,5)) // Nil ()

  println(flatMap (lst)(v => Cons(v+1, Nil()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(flatMap (lst)(v => Cons(v+1, Cons(v+2, Nil()))))
  // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))

  println(map2(lst)(f =>"ciao " + f))

  println(filter[Int](lst)(_ >=20))

  println(max(lst),max(Nil()))

  println("-------FOLD--------")
  println(foldLeft(lst)(0)(_-_))
  println(foldRight(lst)(0)(_-_))
  println("-------ENDFOLD--------")

  import Person._

  //Student(name: String, year: Int)
  //Teacher(name: String, course: String)
  val personList:List[Person] = Cons(Student("Luca",2020), Cons(Teacher("TMario1","Corso1"),
    Cons(Teacher("TMario2","Corso2"), Cons(Teacher("TMario3","Corso3"),
      Cons(Teacher("TMario4","Corso4"), Cons(Student("Marco",2020), Nil()))))))

  val teacher = filter(personList){case Teacher(_,_) => true; case _ => false}
  val courses: List[String] = map(teacher){case Teacher(_,c)=>c}

  println(courses)

  val cFlat:List[String] = flatMap(personList){case Teacher(_,c) => Cons(c,Nil());case _ => Nil()}
  println(cFlat)
}