package AlessandroMartignano.u04

import AlessandroMartignano.u04.Lists._
import AlessandroMartignano.u04.Lists.List._
import AlessandroMartignano.u04.Optionals._
import AlessandroMartignano.u04.Optionals.Option._
import AlessandroMartignano.u04.Streams._
import AlessandroMartignano.u04.Streams.Stream._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = StreamPowerIterator(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = StreamPowerIterator(Stream.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = StreamPowerIterator(take(generate(Random.nextBoolean()))(size))

  private case class StreamPowerIterator[A](var stream: Stream[A]) extends PowerIterator[A] {
    private var list: List[A] = nil

    override def next(): Option[A] = stream match {
      case Stream.Cons(h, t) => {
        stream = t()
        val e = h()
        list = append(list, List.Cons(e, nil))
        Some(e)
      }
      case _ => None()
    }

    override def allSoFar(): List[A] = List.map(list)(x => x)

    override def reversed(): PowerIterator[A] = StreamPowerIterator(Stream.fromList(reverse(allSoFar())))

  }

}

