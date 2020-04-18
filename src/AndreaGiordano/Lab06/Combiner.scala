package AndreaGiordano.Lab06

/** Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly. To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}
case class MyCombiner[A](override val unit:A, private val c:(A,A)=>A) extends Combiner[A]{
  override def combine(a: A, b: A): A = c(a,b)
}
/*
object CombinerSum extends Combiner[Double]{
  override def unit: Double = 0

  override def combine(a: Double, b: Double): Double = a + b
}
object CombinerConcat extends Combiner[String]{
  override def unit: String = ""

  override def combine(a: String, b: String): String = a + b
}
object CombinerMax extends Combiner[Int]{
  override def unit: Int = Int.MinValue

  override def combine(a: Int, b: Int): Int = if(a>b) a else b
}
*/

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = combiner(a)(MyCombiner(0,_+_))
  //override def sum(a: List[Double]): Double = combiner(a)(CombinerSum)

  override def concat(a: Seq[String]): String = combiner(a.toList)(MyCombiner("",_+_))
  //override def concat(a: Seq[String]): String = combiner(a.toList)(CombinerConcat)

  override def max(a: List[Int]): Int = combiner(a)(MyCombiner(Int.MinValue, (x, y)=>if(x>y)x else y))
  //override def max(a: List[Int]): Int = combiner(a)(CombinerMax)

  def combiner[A](a: List[A])(c: Combiner[A]): A = a match {
    case h :: t => c.combine(h,combiner(t)(c))
    case _ => c.unit
  }
}

object TryFunctions extends App {
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648
}