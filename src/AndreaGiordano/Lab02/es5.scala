package AndreaGiordano.Lab02

object es5 extends App {
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  def composeG[A](f: A=> A, g: A => A): A=> A = x => f(g(x))
  def composeGInt[A>:Int](f: A=> A, g: A => A): A=> A = x => f(g(x))

  //val esc: String => String = _ + "!"
  val scala: String => String = _+" Scala"
  println(compose(_-1,_*2)(5))
  println(composeG((a:String)=>a + "!",scala)("Hello"))
  println(composeGInt(_-1,_*2)(5))
}
