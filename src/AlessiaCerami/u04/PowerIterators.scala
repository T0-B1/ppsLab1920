package AlessiaCerami.u04

import Optionals._
import Streams.Stream._
import Optionals.Option._
import Lists.List._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Optionals.Option[A]
  def allSoFar(): Lists.List[A]
  def reversed(): PowerIterator[A]
}

case class PowerIterators[A](private var stream: Streams.Stream[A]) extends PowerIterator[A] {
  private var gen:Lists.List[A] = nil

  override def next(): Optionals.Option[A] = stream match {
    case Streams.Stream.Cons(h, t) =>stream=t(); gen=append(gen,Lists.List.Cons(h(),nil)); Some(h())
    case _ => Option.empty
  }

  override def allSoFar(): Lists.List[A] = gen

  override def reversed(): PowerIterator[A] = PowerIterators(Streams.Stream.fromList[A](gen))
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: Lists.List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}


class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
    PowerIterators(iterate(start)(successive))

  override def fromList[A](list: Lists.List[A]): PowerIterator[A] =
    PowerIterators(Streams.Stream.fromList(reverse(list)))

  override def randomBooleans(size: Int): PowerIterator[Boolean] =
    PowerIterators(take(generate(Random.nextBoolean()))(size))
}
