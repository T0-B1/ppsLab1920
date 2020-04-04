package AlessiaCerami.u03
import AlessiaCerami.u03.Lists.List._

object Modules extends App {
  sealed trait Person
  object Person {

    case class Student(name: String, year: Int) extends Person
    case class Teacher(name: String, course: String) extends Person

    def name(p:Person):String = p match {
      case Student(n,_) => n
      case Teacher(n,_) => n
    }
  }

  import Person._
  val listPerson:Lists.List[Person] = Cons(Student("mario",2015),Cons(Teacher("Viroli","OOP"), Cons(Teacher("Ricci","PCD"),
                              Cons(Student("pippo",2015),Cons(Teacher("Bravetti","LCMC"),
                              Cons(Teacher("Rizzi","Bi"),Nil()))))))

  val res = filter(listPerson) { case Teacher(_, _) => true; case _ => false }

  val course = map(res){ case Teacher(_,c) => c }

  val flatMapCourse:Lists.List[String] = flatMap(listPerson){case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()}
  println(course)
  println(flatMapCourse)
}