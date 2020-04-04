package AndreaGiordano.Lab02

object es7 extends App {
  sealed trait Shape
  object Shape {
    case class Circle(radius:Double) extends Shape
    case class Square(side:Double) extends Shape
    case class Rectangle(side1:Double,side2:Double) extends Shape

    def perimeter(s:Shape):Double = s match {
      case Square(side) => side*4
      case Circle(radius) => radius*3.14*2
      case Rectangle(side1,side2) => side1*2 + side2*2
    }

    def area(s:Shape):Double = s match {
      case Square(side) => side*side
      case Rectangle(side1,side2) => side1*side2
      case Circle(radius) => 3.14*radius*radius
    }
  }

  import Shape._
  val square:Shape = Square(17.531)
  val rectangle:Shape = Rectangle(13.922,21.186)
  val circle:Shape = Circle(11.574)

  println("circle area: " + area(circle))
  println("rectangle perimeter: " + perimeter(rectangle))
  println("square area: " + area(square))
}
