package AlessiaCerami.u03

import AlessiaCerami.u03.Streams.Stream

object Streams {
  import Lists._
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
      case Cons(h, t) => List.Cons(h(), toList(t()))
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

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()
    }

    def iterate[A](init: => A)(next: A => A): Stream[A] = cons(init, iterate(next(init))(next))

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
      case (Cons(_, tail), n) => if (n > 0) drop(tail())(n - 1) else stream
      case _ => Empty()
    }

    def constant[A](k: A): Stream[A] = Stream.cons(k, constant(k))

    def fibonacci(): Stream[Int] = {
      def _fib(prev: Int, current:Int):Stream[Int] = Cons(()=>prev+current, ()=>_fib(current,prev+current))
      Cons(()=>0, ()=>Cons(()=>1, ()=>_fib(0,1)))
    }

  }
}

object StreamsMain extends App {
  // var simplifies chaining of functions a bit..
  var str = Stream.iterate(0)(_+1)   // {0,1,2,3,..}
  str = Stream.map(str)(_+1)    // {1,2,3,4,..}
  str = Stream.filter(str)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  str = Stream.take(str)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str)) // [1,2,21,22,..,28]

  val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  // DROP WITH STREAM
  val s = Stream.take(Stream.iterate(0)(_+1))(10)
  println(Stream.toList(Stream.drop(s)(6))) //Cons(6,Cons(7,Cons(8,Cons(9,Nil()))))
  println(Stream.toList(Stream.drop(s)(0))) //tutto lo stream

  println(Stream.toList((Stream.take(Stream.constant("x"))(5))))

  val fibs:Stream[Int] = Stream.fibonacci()
  println(Stream.toList(Stream.take(fibs)(8)))
}
