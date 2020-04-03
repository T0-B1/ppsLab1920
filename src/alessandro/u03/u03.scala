package alessandro.u03

import source.u02.Modules.Person
import source.u02.Modules.Person.{Student, Teacher}
import source.u02.Optionals.Option
import source.u02.Optionals.Option.{None, Some}
import source.u03.Lists.List._
import source.u03.Lists._

import scala.annotation.tailrec

object Solution {

  // 1

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n == 0 => l
    case Cons(h, t) => drop(t, n-1)
    case _ => Nil()
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()
  }

  def mapFlat[A,B](l: List[A])(mapper: A=>B): List[B] =
    flatMap(l)((x: A) => Cons(mapper(x), Nil()))

  def filterFlat[A](l1: List[A])(pred: A=>Boolean): List[A] = {
    def f(e:A) : List[A] = e match {
      case e if pred(e) => Cons(e, Nil())
      case _ => Nil()
    }
    flatMap(l1)(f)
  }

  // 2

  def max(l: List[Int]): Option[Int] = {
    def _max(l2: List[Int], curMax: Option[Int]) : Option[Int] = (l2, curMax) match {
      case (Cons(h, t), Some(m)) => if (h > m) _max(t, Some(h)) else _max(t, Some(m))
      case (Cons(h, t), None()) => _max(t, Some(h))
      case _ => curMax
    }
    _max(l, None())
  }

  // 3

  def teacherCourses(l: List[Person]): List[String] =
    flatMap(l)(p => p match {
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    })

  // 4

  @tailrec
  def foldLeft[A,B](l: List[A])(acc: B)(f: (B,A)=>B): B = l match {
    case Cons(h,t) => foldLeft(t)(f(acc,h))(f)
    case Nil() => acc
  }

  def foldRight[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B = l match {
    case Cons(h,t) => f(h, foldRight(t)(acc)(f))
    case Nil() => acc
  }

  // Stack may overflow
  def foldRightTailRec[A](l: List[A])(default: A)(op: (A, A) => A) : A = {
    def compose(f: A => A, g: A => A): A => A = x => f(g(x))
    def genOpInv(o: (A, A) => A, i: A) : A => A = o(i, _)
    @annotation.tailrec
    def frt(l: List[A])(c: A => A) : A => A = l match {
      case Cons(h, t) => frt(t)(compose(c, genOpInv(op, h)))
      case _ => c
    }
    frt(l)(x => x)(default)
  }

}

sealed trait Stream[A]
object Stream {
  private case class Empty[A]() extends Stream[A]
  private case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  def empty[A](): Stream[A] = Empty()

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def toList[A](stream: Stream[A]): List[A] = stream match {
    case Cons(h,t) => List.Cons(h(), toList(t()))
    case _ => List.Nil()
  }

  def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match {
    case Cons(head, tail) => cons(f(head()), map(tail())(f))
    case _ => Empty()
  }

  def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match {
    case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
    case Cons(head, tail) => filter(tail())(pred)
    case _ => Empty()
  }

  def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
    case (Cons(head, tail), n) if n>0 => cons(head(), take(tail())(n - 1))
    case _ => Empty()
  }

  def iterate[A](init: => A)(next: A => A): Stream[A] = cons(init, iterate(next(init))(next))

  // 5

  def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
    case (Cons(head, tail), n) if n>0 => drop(tail())(n-1)
    case (Cons(head, tail), n) => stream
    case _ => Empty()
  }

  // 6

  def constant[A](k: A): Stream[A] = Stream.cons(k, constant(k))

  // 7

  def fibo(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fibo(n-1) + fibo(n-2)
  }

  def fiboTR(n: Int): Int = {
    @annotation.tailrec  // checks only if optimisation is possible
    def _fibo(a: Int, b: Int, n: Int): Int = n match {
      case 0 => a
      case _ => _fibo(b, a+b, n-1)
    }
    _fibo(0, 1, n)
  }

  def fiboStream(): Stream[Int] = map(iterate(0)(_+1))(fiboTR)

}

object test extends App {
  import Solution._
  val lst = Cons (10 , Cons (20 , Cons (30 , Nil ())))
  println(drop (lst ,1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop (lst ,2)) // Cons (30 , Nil ())
  println(drop (lst ,5)) // Nil ()

  println(flatMap (lst )(v => Cons (v+1, Nil ()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(flatMap (lst )(v => Cons (v+1, Cons (v+2, Nil ()))))
  // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))

  println(filterFlat[Int](lst)(_ >=20)) // 20,30

  println(max( Cons (10 , Cons (25 , Cons (20 , Nil ()))))) // Some (25)
  println(max(Nil ())) // None ()

  val people: List[Person] = Cons(Student("Mario", 0), Cons(Teacher("Wario", "PPS"), Cons(Teacher("Yoshi", "PCD"), Nil())))
  println(teacherCourses(people))

  val lst2 = Cons (3, Cons (7, Cons (1, Cons (5, Nil ()))))
  println(foldLeft (lst2)(0)(_-_)) // -16
  println(foldRight (lst2)(0)(_-_)) // -8
  println(foldRightTailRec (lst2)(0)(_-_)) // -8

  val s = Stream.take(Stream.iterate(0)(_+1))(10)
  println(Stream.toList(Stream.drop(s)(6)))
  // => Cons (6, Cons (7, Cons (8, Cons (9, Nil ()))))

  println(Stream.toList(Stream.take(Stream.constant("x"))(5)))
  // => Cons (x, Cons (x, Cons (x, Cons (x, Cons (x, Nil ())))))

  val fibs : Stream [Int ] = Stream.fiboStream()
  println(Stream.toList(Stream.take(fibs)(8)))
  // => Cons (0, Cons (1, Cons (1, Cons (2, Cons (3, Cons (5, Cons (8, Cons (13 , Nil ()))))))))
}
