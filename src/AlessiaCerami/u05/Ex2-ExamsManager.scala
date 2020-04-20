package AlessiaCerami.u05

object Kind extends Enumeration {
  val RETIRED, FAILED, SUCCEEDED = Value
}

case class ExamResult(kind:Kind.Value, eval:Int = 0, laude:Boolean = false)


sealed trait ExamResultFactory {

  def failed:ExamResult

  def retired:ExamResult

  def succeededCumLaude: ExamResult

  def succeeded(evaluation: Int): ExamResult

}

case class ExamResultFactoryImpl() extends ExamResultFactory{

  override def failed: ExamResult = ExamResult(Kind.FAILED)

  override def retired: ExamResult = ExamResult(Kind.RETIRED)

  override def succeededCumLaude: ExamResult = ExamResult(Kind.SUCCEEDED,30,true)

  override def succeeded(evaluation: Int): ExamResult = ExamResult(Kind.SUCCEEDED,evaluation)
}

sealed trait ExamsManager {

  def createNewCall(call:String):Unit

  def addStudentResult(call:String, student:String, result:ExamResult): Unit

  def getAllStudentsFromCall(call:String):Set[String]

  def getEvaluationsMapFromCall(call: String):Map[String,ExamResult]

  def getResultsMapFromStudent(student: String):Map[String,ExamResult]

  def getBestResultFromStudent(student: String):Option[Int]

}

object ExamsManagerImpl extends ExamsManager{

  var studRes:Map[String,Map[String,ExamResult]] = Map.empty

  override def createNewCall(call:String):Unit = studRes = studRes + (call -> (Map.empty[String,ExamResult]))

  override def addStudentResult(call:String, student:String, result:ExamResult):Unit =
    studRes = studRes + (call-> (studRes(call) + (student -> result)))

  override def getAllStudentsFromCall(call:String):Set[String] = studRes(call).keySet

  override def getEvaluationsMapFromCall(call:String):Map[String, ExamResult] = studRes(call)

  override def getResultsMapFromStudent(student:String):Map[String, ExamResult] =
    studRes.collect({case (exName,exam) if(exam.contains(student)) => (exName -> exam(student))})

  override def getBestResultFromStudent(student:String):Option[Int] = {
    studRes.filter(x => x._2.contains(student) && x._2(student).kind == Kind.SUCCEEDED)
      .foldLeft(Option.empty[Int])((opt,exam) => if(exam._2(student).eval > opt.getOrElse(0))
        Some(exam._2(student).eval) else opt)
  }
}
object ExamsManagerTest extends App {

  val s1 = "Student1"
  val s2 = "Student2"
  val s3 = "Student3"

  val e1 = "Exam1"
  val e2 = "Exam2"
  val e3 = "Exam3"

  ExamsManagerImpl.createNewCall(e1)
  ExamsManagerImpl.createNewCall(e2)
  ExamsManagerImpl.createNewCall(e3)
  
  val resFactory = ExamResultFactoryImpl()
  ExamsManagerImpl.addStudentResult(e1,s1, resFactory.succeeded(25))
  ExamsManagerImpl.addStudentResult(e2,s1, resFactory.succeeded(28))
  ExamsManagerImpl.addStudentResult(e3,s1, resFactory.succeeded(20))
  ExamsManagerImpl.addStudentResult(e3,s2, resFactory.succeeded(30))
  ExamsManagerImpl.addStudentResult(e1,s3, resFactory.succeeded(28))

  println(ExamsManagerImpl.getAllStudentsFromCall(e1))
  println(ExamsManagerImpl.getEvaluationsMapFromCall(e1))
  println(ExamsManagerImpl.getResultsMapFromStudent(s1))
  println(ExamsManagerImpl.getBestResultFromStudent(s1))
}