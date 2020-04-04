package AlessiaCerami.u02

object es7 extends App {
  sealed trait Shape

  object Shape {
    case class Rectangle(base:Double, height:Double) extends Shape
    case class Circle(radius:Double) extends Shape
    case class Square(side:Double) extends Shape

    def perimeter(s:Shape):Double = s match{
      case Rectangle(base,height) => base*2+height+2
      case Circle(radius) => radius*2*3.14
      case Square(side) => side*4
    }

    def area(s:Shape):Double = s match{
      case Rectangle(base,height) => base*height
      case Circle(radius) => 3.14*radius*radius
      case Square(side) => side*side
    }
}
  import Shape._
  println("Rectangle Perimeter--> "+perimeter(Rectangle(12.34,23)))
  println("Rectangle Area--> "+area(Rectangle(12.34,23)))
}
