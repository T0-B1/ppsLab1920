package u05lab.code

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
  val r = scala.util.Random
  import PerformanceUtils._ //Comparison

  /* Linear sequences: List, ListBuffer */

  /* Indexed sequences: Vector, Array, ArrayBuffer */

  /* Sets */

  /* Maps */

  val mutLis:scala.collection.mutable.ListBuffer[Int] = ListBuffer.empty[Int]
  var immLis:scala.collection.immutable.List[Int] = scala.collection.immutable.List.empty[Int]

  measure("Mutable Add"){mutLis.addAll(1 to 1000000)}
  measure("Immutable Add"){immLis = immLis.:++(1 to 1000000)}

  println("Mutable list size "+mutLis.size)
  println("Immutable list size "+immLis.size)

  measure("Mutable Get"){(0 until 10000).foreach(_ => mutLis(r.nextInt(1000000)))}
  measure("Immutable Get"){(0 until 10000).foreach(_ => immLis(r.nextInt(1000000)))}

  measure("Mutable Filter"){mutLis.filterInPlace(x => x < 300 && x < 500)}
  measure("Immutable Filter"){immLis.filter(x => x < 300 && x < 500)}

  measure("Mutable Map"){mutLis.map(x => x < 300 && x < 500)}
  measure("Immutable Map"){immLis.map(x => x < 300 && x < 500)}

  measure("Mutable Drop" ){mutLis.dropWhile( x => x < 300000) }
  measure("Immutable Drop" ){immLis.dropWhile( x => x < 300000) }
  
}