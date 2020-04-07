package AlessiaCerami.u04

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

/** Hints:
 * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
 * - check that equality and toString do not work
 * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
 * - check equality and toString now
 */
object Complex {
  def apply(re:Double, im:Double):Complex = ComplexImpl(re, im) // Fill here

  case class ComplexImpl(var re:Double, var im:Double) extends Complex {

    //z = a + ib
    //w = c + id
    //(a+c) + i(b + d)
    override def +(c: Complex): Complex = new ComplexImpl(re + c.re, im + c.im)

    //(ac-bd)+i(ad+bc)
    override def *(c: Complex): Complex = new ComplexImpl((this.re*c.re-this.im*c.im), (this.re*c.im+this.im+c.re))
  }
}

object TryComplex extends App {
  val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im ) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}

