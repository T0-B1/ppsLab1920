package source.u02

object Modules extends App {

  // An ADT: type + module
  sealed trait Person
  object Person {

    case class Student(name: String, year: Int) extends Person

    case class Teacher(name: String, course: String) extends Person

    def name(p: Person): String = p match {
      case Student(n, _) => n
      case Teacher(n, _) => n
    }
  }

  println(Person.name(Person.Student("mario",2015)))

  import Person._
  println(name(Student("mario",2015)))

}
