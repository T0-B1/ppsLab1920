package AlessiaCerami.u04

import AlessiaCerami.u04.Lists._
import AlessiaCerami.u04.Lists.List._
import AlessiaCerami.u04.Lists.List.Cons // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String
  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = new StudentImpl(name, year)

  private class StudentImpl(val name:String, val year:Int) extends Student {
    private var courseList:List[Course] = nil

    //override def enrolling(course: Course): Unit = courseList = Cons(course, courseList)
    override def enrolling(course: Course*): Unit = {
      //courseList = Cons(course, courseList)
      for(x <- course) courseList = Cons(x, courseList)
    }

    override def courses: List[String] = map(courseList)(x => x.name)

    override def hasTeacher(teacher: String): Boolean = contains(map(courseList)(x => x.teacher),teacher)
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name,teacher)

  case class CourseImpl(var name: String, var teacher: String) extends Course
}

object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  val s4 = Student("Pino")
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  s4.enrolling(cPCD,cPPS)
  println(s1.courses, s2.courses, s3.courses, s4.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

}

/** Hints:
  * - simply implement Course, e.g. with a case class ----
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */
