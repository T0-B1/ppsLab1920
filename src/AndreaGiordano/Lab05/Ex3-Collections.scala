package AndreaGiordano.Lab05

import java.util.concurrent.TimeUnit

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
  /* Comparison */
  import PerformanceUtils._

  /* Linear sequences: List, ListBuffer */

  /* Indexed sequences: Vector, Array, ArrayBuffer */

  /* Sets */

  /* Maps */
  var immMap = scala.collection.immutable.HashMap[Int,Int]()
  val mutMap = scala.collection.mutable.HashMap[Int,Int]()

  measure("ImmutableMap add"){(1 to 1000000).foreach(x=>immMap=immMap.+(x->x))}
  measure("MutableMap add"){(1 to 1000000).foreach(x=>mutMap.+=(x->x))}

  println("size mutable: "+mutMap.size)

  measure("ImmutableMap filter"){immMap.filter(e => e._1 * e._2 % 2 == 0)}
  measure("MutableMap filter"){mutMap.filter(e => e._1 * e._2 % 2 == 0)}

  println("size mutable: "+mutMap.size)

  measure("ImmutableMap map"){immMap.map(e=>(e._1,e._2+1))}
  measure("MutableMap map"){mutMap.map(e=>(e._1,e._2+1))}

  println("size mutable: "+mutMap.size)

  measure("ImmutableMap drop"){immMap.dropWhile(e=>e._2<300000)}
  measure("MutableMap drop"){mutMap.dropWhile(e=>e._2<300000)}

  println("size mutable: "+mutMap.size)

  val rndGen = scala.util.Random

  measure("ImmutableMap random read"){(0 until 100000).foreach(_ => immMap(rndGen.nextInt(1000000)))}
  measure("MutableMap random read"){(0 until 100000).foreach(_ => mutMap(rndGen.nextInt(1000000)))}
  /*
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert( measure("lst last"){ lst.last } > measure("vec last"){ vec.last } )
  */
}