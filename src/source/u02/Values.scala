package source.u02

object Values extends App {

  // values can be associated to names
  // namely, non modifiable variables (of course, there's no side-effects)
  val v = 1

  // types are optional, they are typically always inferred
  val w: Int = 1

  // a procedure to print on console (a hack, not pure FP)
  println(v)

  // Java-style concatenation, semi-colon is optional on Carriage Return
  println("result is " + v);

  // primitive types as expected over the JVM
  val i: Int = 10 + 5 // as in Java, to be read +(10,5)
  val l: Long = 100000000000L // as in Java
  val d: Double = 5.4 * 1.3 // as in Java
  val f: Float = 3.0f // as in Java
  val b: Boolean = true && false // as in Java
  val s: String = "hello" concat " to all" // String methods as operators
  val n: String = null // null can be passed to "objects", sorry

  println(i, l, d, f, b, s, n) // println has a var-arg

}
