package AlessandroMartignano.u05

import java.util.concurrent.TimeUnit

import scala.collection.mutable._
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {

  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}


object CollectionsTest extends App {

  import PerformanceUtils._

  val i = (1 to 1000000)

  /* Linear sequences: List, ListBuffer */
  val lst = i.toList
  val lstBf = ListBuffer.from(i)

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val vec = Vector.from(i)
  val arr = Array.from(i)
  val arrBff = ArrayBuffer.from(i)

  /* Sets */
  val mutSet = scala.collection.mutable.Set.from(i)
  var immSet = scala.collection.immutable.Set.from(i)

  /* Maps */
  val mutMap = scala.collection.mutable.Map.from(i.map( x => (x, x)))
  var immMap = scala.collection.immutable.Map.from(i.map( x => (x, x)))

  /* Comparison */

  measure("lst last"){ lst.last }
  measure("lstBf last"){ lstBf.last }
  measure("vec last"){ vec.last }
  measure("arr last"){ arr.last }
  measure("arrBff last"){ arrBff.last }
  measure("mutSet last"){ mutSet.last }
  measure("immSet last"){ immSet.last }
  measure("mutMap last"){ mutMap.last }
  measure("immMap last"){ immMap.last }

  measure("lst size"){ lst.size }
  measure("lstBf size"){ lstBf.size }
  measure("vec size"){ vec.size }
  measure("arr size"){ arr.size }
  measure("arrBff size"){ arrBff.size }
  measure("mutSet size"){ mutSet.size }
  measure("immSet size"){ immSet.size }
  measure("mutMap size"){ mutMap.size }
  measure("immMap size"){ immMap.size }

  measure("lst contains(42)"){ lst.contains(42) }
  measure("lstBf contains(42)"){ lstBf.contains(42) }
  measure("vec contains(42)"){ vec.contains(42) }
  measure("arr contains(42)"){ arr.contains(42) }
  measure("arrBff contains(42)"){ arrBff.contains(42) }
  measure("mutSet contains(42)"){ mutSet.contains(42) }
  measure("immSet contains(42)"){ immSet.contains(42) }
  measure("mutMap contains(42)"){ mutMap.contains(42) }
  measure("immMap contains(42)"){ immMap.contains(42) }

  measure("lst drop(42)"){ lst.drop(42) }
  measure("lstBf drop(42)"){ lstBf.drop(42) }
  measure("vec drop(42)"){ vec.drop(42) }
  measure("arr drop(42)"){ arr.drop(42) }
  measure("arrBff drop(42)"){ arrBff.drop(42) }
  measure("mutSet drop(42)"){ mutSet.drop(42) }
  measure("immSet drop(42)"){ immSet.drop(42) }
  measure("mutMap drop(42)"){ mutMap.drop(42) }
  measure("immMap drop(42)"){ immMap.drop(42) }

  measure("lst :+(42)"){ lst :+ 42 }
  measure("lstBf :+(42)"){ lstBf :+ 42 }
  measure("vec :+(42)"){ vec :+ 42 }
  measure("arr :+(42)"){ arr :+ 42 }
  measure("arrBff :+(42)"){ arrBff :+ 42  }
  measure("mutSet :+(42)"){ mutSet += 42  }
  measure("immSet :+(42)"){ immSet = immSet + 42 }
  measure("mutMap :+(42)"){ mutMap += (42 -> 42) }
  measure("immMap :+(42)"){ immMap = immMap + (42 -> 42) }

  /*
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert( measure("lst last"){ lst.last } > measure("vec last"){ vec.last } )

   */

}