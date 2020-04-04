package AndreaGiordano.Lab03

  // An ADT: type + module
sealed trait Person
object Person {

  case class Student(name:String, year:Int) extends Person

  case class Teacher(name:String, course:String) extends Person

  def name(p: Person):String = p match {
    case Student(n,_) => n
    case Teacher(n,_) => n
  }

  def isTeacher(p:Person):Boolean = p match{
    case Teacher(_,_) => true
    case _ => false
  }
}

