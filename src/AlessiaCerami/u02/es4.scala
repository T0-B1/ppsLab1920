package AlessiaCerami.u02

object es4 extends App {

  val p1:(Int,Int,Int) => Boolean = (x , y, z) => y <= x && y >= z
  val p2: (Int) => (Int) => (Int) => Boolean = x => y => z => y <= x && y >= z
  def p3(x:Int, y:Int, z:Int):Boolean = y <= x && y >= z
  def p4(x:Int) (y:Int) (z:Int):Boolean = y <= x && y >= z

  println(p2(3)(2)(1))
  println(p2(3)(2)(8))
  println(p4(3)(2)(1))
  println(p4(3)(2)(8))

}
