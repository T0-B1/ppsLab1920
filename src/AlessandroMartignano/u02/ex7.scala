package AlessandroMartignano.u02

object ex7 extends App {

  sealed trait Shape
  object Shape {

    case class Circle(radius: Double) extends Shape
    case class Square(sideLength: Double) extends Shape
    case class Rectangle(base: Double, height: Double) extends Shape

    def perimeter(s: Shape): Double = s match {
      case Circle(r) => 2*r*Math.PI
      case Square(s) => 4*s
      case Rectangle(b, h) => 2*(b+h)
    }

    def area(s: Shape): Double = s match {
      case Circle(r) => Math.PI*Math.pow(r, 2)
      case Square(s) => Math.pow(s, 2)
      case Rectangle(b, h) => b*h
    }

  }

  import Shape._

  val c = Circle(1.0)
  val s = Square(2.0)
  val r = Rectangle(2.0, 4.0)

  println(area(c))
  println(perimeter(c))
  println(area(s))
  println(perimeter(s))
  println(area(r))
  println(perimeter(r))

}
