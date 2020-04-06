package AndreaGiordano.Lab04

import AndreaGiordano.Lab04.Lists._
import AndreaGiordano.Lab04.Lists.List._
import AndreaGiordano.Lab04.Lists.List.Cons // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(courses: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}
object Student {
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name,year)
  case class StudentImpl(var name:String, override val year:Int) extends Student {

    private var _courses:List[Course] = nil

    override def enrolling(courses: Course*):Unit = {
      //_courses = Cons(course,_courses)
      for ( c <- courses )
        _courses = Cons(c,_courses)
    }

    override def courses: List[String] = map(_courses)(c=>c.name)

    override def hasTeacher(teacher: String): Boolean = contains(map(_courses)(c=>c.teacher),teacher)
  }
}

trait Course {
  def name: String
  def teacher: String
}
object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name,teacher)
  case class CourseImpl(var name:String, var teacher:String) extends Course
}

object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  val s4 = Student("ale",2020)

  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  s4.enrolling(cPCD,cPPS,cSDR)
  println("S1: "+s1.courses, " S2: "+s2.courses, " S3: "+s3.courses)
  println("S4: "+s4.courses)
  // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
}

/** Hints:
  * - simply implement Course, e.g. with a case class
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */
