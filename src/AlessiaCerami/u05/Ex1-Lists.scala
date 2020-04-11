package AlessiaCerami.u05

import scala.annotation.tailrec
import scala.language.postfixOps // silence warnings

sealed trait List[A] {
  def collect[B](pF:PartialFunction[A,B]):List[B]

  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def toSeq: Seq[A]

  def foldLeft[B](acc: B)(f: (B,A)=>B): B

  def foldRight[B](acc: B)(f: (A,B)=>B): B

  def flatMap[B](f: A => List[B]): List[B]

  def reverse(): List[A]

  def zipRight: List[(A,Int)]

  def partition(pred: A => Boolean): (List[A],List[A])

  def span(pred: A => Boolean): (List[A],List[A])

  def reduce(op: (A,A)=>A): A

  def takeRight(n: Int): List[A]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A]

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

  override def head: Option[A] = this match {
    case h :: t => Some(h)
    case _ => None
  }
  override def tail: Option[List[A]] = this match {
    case h :: t => Some(t)
    case _ => None
  }
  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }
  override def foreach(consumer: (A)=>Unit): Unit = this match {
    case h :: t => {consumer(h); t foreach consumer}
    case _ => None
  }
  override def get(pos: Int): Option[A] = this match {
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t get (pos-1)
    case _ => None
  }
  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if (predicate(h)) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }
  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def foldLeft[B](acc: B)(f: (B,A)=>B): B = this match {
    case Cons(h,t) => t.foldLeft(f(acc,h))(f)
    case Nil() => acc
  }

  override def foldRight[B](acc: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(acc)((acc,elem) => f(elem,acc))

  override def reverse(): List[A] =
    this.foldLeft(Nil[A].asInstanceOf[List[A]])((acc,elem) => Cons(elem,acc))

  override def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(h,t) => f(h).append(t.flatMap(f))
    case Nil() => Nil()
  }

  override def zipRight: List[(A,Int)] = {
    def _zipRight(l:List[A], arg:Int):List[(A,Int)] = l match{
      case Cons(h,t) => Cons((h,arg), _zipRight(t,arg+1))
      case Nil() => Nil()
    }
    _zipRight(this,0)
  } // questions: what is the type of keyword ???

  override def partition(pred: A => Boolean):(List[A],List[A]) = (this.filter(pred), this.filter(x => !pred(x)))

  override def span(pred: A => Boolean): (List[A],List[A]) = {
    def _span(leftList:List[A],rightList:List[A]):(List[A],List[A]) = rightList match {
      case Cons(h,t) if pred(h) => _span(leftList.::(h),t)
      case _ => (leftList,rightList)
    }
    _span(Nil(),this)
  }

  /**
    *
    * @throws UnsupportedOperationException if the list is empty
    */
  override def reduce(op: (A,A)=>A): A = this match {
    case Cons(_head, _tail) => _tail.foldLeft(_head)(op)
    case _ => throw new UnsupportedOperationException
    /*def _reduce(l:List[A], e:A): A = l match {
      case Cons(h, t) => op(_reduce(t,e),h)
      case _ => e
    }
    this match {
      case Cons(_head, _tail) => _reduce(_tail,_head)
      case _ => throw new UnsupportedOperationException
    }*/
  }

  override def takeRight(n: Int): List[A] = {
    def _takeRight(l:List[A]):(List[A],Int) = l match {
      case Cons(h,t) => {
        val res = _takeRight(t)
        if(res._2 < n) (Cons(h,res._1),res._2+1) else (res._1,n)
      }
      case _ => (Nil(),0)
    }
    _takeRight(this)._1
  }

  override def collect[B](pF:PartialFunction[A,B]):List[B] = this filter pF.isDefinedAt map pF
}

// Factories
object List {

  // Smart constructors
  def nil[A]: List[A] = Nil()
  def cons[A](h: A, t: List[A]): List[A] = Cons(h,t)

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}

object ListsTest extends App {

  import List._  // Working with the above lists
  println(List(10,20,30,40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // 10
  println(l.tail) // 20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l append l toSeq) // as a list: 10,20,30,40,10,20,30,40
  println(l get 2) // 30
  println(of("a",10)) // a,a,a,..,a
  println(l filter (_<=20) map ("a"+_) ) // a10, a20

  assert(List(1,2,3) == List(1,2,3))

  println(scala.collection.immutable.List(10,20,30,40).partition(_>15))
  println(scala.collection.immutable.List(10,20,30,40).span(_>15))

  // Ex. 1: zipRight
  println("Ex. 1: zipRight --> "+ l.zipRight.toSeq) // List((10,0), (20,1), (30,2), (40,3))

  // Ex. 2: partition
  println("Ex. 2: partition --> "+ l.partition(_>15)) // ( Cons(20,Cons(30,Cons(40,Nil()))), Cons(10,Nil()) )

  // Ex. 3: span
  println("Ex. 3: span --> "+l.span(_>15)) // ( Nil(), Cons(10,Cons(20,Cons(30,Cons(40,Nil())))) )
  println("Ex. 3: span --> "+l.span(_<15)) // ( Cons(10,Nil()), Cons(20,Cons(30,Cons(40,Nil()))) )

  // Ex. 4: reduce
  println("Ex. 4: reduce --> "+l.reduce(_+_)) // 100
  println("Ex. 4: reduce --> "+l.reduce(_-_)) // -80
  try { List[Int]().reduce(_+_); assert(false) } catch { case _:UnsupportedOperationException => }

  // Ex. 5: takeRight
  println("Ex. 5: takeRight --> "+l.takeRight(2)) // Cons(30,Cons(40,Nil()))
  println("Ex. 5: takeRight --> "+l.takeRight(-1)) // Cons(30,Cons(40,Nil()))

  // Ex. 6: collect
  println("Ex. 6: collect --> "+l.collect { case x if x<15 || x>35 => x-1 }) // Cons(9, Cons(39, Nil()))
}