package AndreaGiordano.Lab02

object es4 extends App {
  val noCurried: Int => Int => Int => Boolean = x => y => z => x >= y && y >= z
  val curried: (Int,Int,Int)=>Boolean = (x,y,z)=> x>=y && y>=z
  def noCurriedD(x:Int)(y:Int)(z:Int):Boolean = x>=y && y>=z
  def curriedD(x:Int, y:Int, z:Int):Boolean = x>=y && y>=z

  println(noCurried(5)(4)(3), noCurried(5)(6)(3))
  println(curried(5,4,3), curried(5,6,3))
  println(noCurriedD(5)(4)(3), noCurriedD(5)(6)(3))
  println(curriedD(5,4,3), curriedD(5,6,3))

}
