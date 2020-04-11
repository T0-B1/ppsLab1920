package AndreaGiordano.Lab05

object Kind extends Enumeration{
  val RETIRED, FAILED, SUCCEEDED = Value
}

case class ExamResult(kind:Kind.Value, grade:Int = 0, laude:Boolean = false)

trait ExamResultFactory {
  def failed:ExamResult

  def retired:ExamResult

  def succeededCumLaude:ExamResult

  def succeeded(evaluation: Int):ExamResult
}
object ExamResultFactory extends ExamResultFactory {
  override def failed:ExamResult = ExamResult(Kind.FAILED)

  override def retired:ExamResult = ExamResult(Kind.RETIRED)

  override def succeededCumLaude:ExamResult = ExamResult(Kind.SUCCEEDED,30,laude = true)

  override def succeeded(evaluation: Int):ExamResult = ExamResult(Kind.SUCCEEDED,evaluation)
}

sealed trait ExamsManager {
  def createNewCall(call: String): Unit

  def addStudentResult(call: String, student: String, result: ExamResult): Unit

  def getAllStudentsFromCall(call: String):Set[String]

  def getEvaluationsMapFromCall(call: String):Map[String,ExamResult]

  def getResultsMapFromStudent(student: String):Map[String,ExamResult]

  def getBestResultFromStudent(student: String):Option[Int]
}
case class ExamsManagerImpl() extends ExamsManager{
  private var examsMap:Map[String,Map[String,ExamResult]] = Map.empty[String,Map[String,ExamResult]]

  override def createNewCall(call:String):Unit = examsMap = examsMap + (call -> Map.empty[String,ExamResult])

  override def addStudentResult(call:String, student:String, result:ExamResult):Unit =
    examsMap = examsMap + (call -> (examsMap(call) + (student -> result)))

  override def getAllStudentsFromCall(call:String):Set[String] = examsMap(call).keySet

  override def getEvaluationsMapFromCall(call:String):Map[String, ExamResult] = examsMap(call)

  override def getResultsMapFromStudent(student:String):Map[String, ExamResult] =
    examsMap.collect({case (exName,exam) if exam.contains(student) => exName->exam(student)})
  /*{
    val res = Map.empty[String,ExamResult]
    examsMap.foreach(ex=> {
      if(ex._2.contains(student))
      res + (ex._1 -> ex._2(student))
    })

  }*/

  override def getBestResultFromStudent(student:String):Option[Int] =
    examsMap.filter(x=> x._2.contains(student) && x._2(student).kind==Kind.SUCCEEDED)
      .foldLeft(Option.empty[Int])((opt,ex)=> { val g = ex._2(student).grade
      if( g > opt.get) Option(g) else opt })
  /*
  .foldLeft(Option.empty[Int]){
      case (opt,ex) if ex._2(student).grade > opt.get => Option(ex._2(student).grade)
      case (opt,_) => opt
    }
  */
}

object ExamsManagerTest extends App {

  /* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */
  val manager = ExamsManagerImpl()
  val s1 = "student1"
  val s2 = "student2"
  val s3 = "student3"
  val e1 = "exam1"
  val e2 = "exam2"
  val e3 = "exam3"
  manager.createNewCall(e1)
  manager.createNewCall(e2)
  manager.createNewCall(e3)
  manager.addStudentResult(e1,s1,ExamResultFactory.succeeded(25))
  manager.addStudentResult(e1,s2,ExamResultFactory.failed)
  manager.addStudentResult(e1,s3,ExamResultFactory.succeededCumLaude)
  manager.addStudentResult(e2,s1,ExamResultFactory.succeededCumLaude)
  manager.addStudentResult(e2,s2,ExamResultFactory.failed)
  manager.addStudentResult(e2,s3,ExamResultFactory.retired)
  manager.addStudentResult(e3,s1,ExamResultFactory.succeeded(28))
  manager.addStudentResult(e3,s2,ExamResultFactory.succeeded(18))
  manager.addStudentResult(e3,s3,ExamResultFactory.failed)
  println(manager.getAllStudentsFromCall(e1))
  println(manager.getResultsMapFromStudent(s1))
  println(manager.getBestResultFromStudent(s1))
  println(manager.getEvaluationsMapFromCall(e1))
}