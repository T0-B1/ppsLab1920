package AlessiaCerami.u02

object es5 extends App {
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def composeWithGenerics[A](f: A => A, g: A => A): A => A = x => f(g(x))
  def compose2[A>:Int](f: A => A, g: A => A): A => A = x => f(g(x))

  val s:String => String = _+"!"
  val s2:String => String = _+" Scala"
  println(compose(_-1,_*2)(5))
  println(composeWithGenerics(s,s2)("Hello"))
  println(compose2(_-1,_*2)(5))
}
