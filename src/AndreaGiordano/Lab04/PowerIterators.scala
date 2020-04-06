package AndreaGiordano.Lab04

import Optionals.Option._
import Lists.List._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Optionals.Option[A]
  def allSoFar(): Lists.List[A]
  def reversed(): PowerIterator[A]
}

import Streams.Stream._
case class PowerIterators[A](private var stream:Streams.Stream[A]) extends PowerIterator[A]{

  private var passed:Lists.List[A] = nil

  override def next(): Optionals.Option[A] = stream match {
    case Streams.Stream.Cons(h,t) => stream=t();passed=append(passed,Lists.List.Cons(h(),nil)); Some(h())
    case _ => Optionals.Option.empty
  }

  override def allSoFar(): Lists.List[A] = Lists.List.map(passed)(x=>x)

  override def reversed(): PowerIterator[A] = PowerIterators(Streams.Stream.fromList[A](passed))
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: Lists.List[A]):PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int):PowerIterator[Int] =
    PowerIterators(Streams.Stream.iterate(start)(successive))

  override def fromList[A](l: Lists.List[A]):PowerIterator[A] = PowerIterators(Streams.Stream.fromList[A](reverse(l)))

  override def randomBooleans(size: Int):PowerIterator[Boolean] = PowerIterators(take(generate(Random.nextBoolean()))(size))
}
