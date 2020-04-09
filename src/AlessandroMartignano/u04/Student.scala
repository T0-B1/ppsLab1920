package AlessandroMartignano.u04

import AlessandroMartignano.u04.Lists._
import AlessandroMartignano.u04.Lists.List._
import AlessandroMartignano.u04.Lists.List.Cons

import scala.Option
import scala.annotation.tailrec

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
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

  private case class StudentImpl(name: String, year: Int) extends Student {

    private var _courses:List[Course] = nil

    override def enrolling(courses: Course*):Unit = {
      for ( c <- courses )
        _courses = Cons(c,_courses)
    }
    override def courses: List[String] = map(_courses)(c => c.name)

    override def hasTeacher(teacher: String): Boolean = contains(map(_courses)(c => c.teacher))(teacher)
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

  private case class CourseImpl(name: String, teacher: String) extends Course
}

object sameTeacher {
  def unapply(courses: List[Course]): scala.Option[String] = allEquals(map(courses)(c=>c.teacher))
}

object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

  var l = Cons(Course("PPS1","Viroli"), Cons(Course("PPS2","Viroli"), Cons(Course("PPS3","Viroli"), Nil())))
  l match {
    case sameTeacher(t) => println (s"courses: $l share the same teacher: $t")
    case _ => println (s"courses: $l have different teachers")
  }
  val l2 = Cons(cPPS, Cons(cPCD, Nil()))
  l2 match {
    case sameTeacher(t) => println (s"courses: $l2 share the same teacher: $t")
    case _ => println (s"courses: $l2 have different teachers")
  }

}

/** Hints:
  * - simply implement Course, e.g. with a case class
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */
