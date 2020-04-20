package AlessandroMartignano.u05

object Kind extends Enumeration {
  val RETIRED, FAILED, SUCCEEDED = Value
}

trait ExamResult {
  def getKind: Kind.Value

  def getEvaluation: Option[Int]

  def cumLaude: Boolean
}

case class ExamResultImpl(kind: Kind.Value, eval: Option[Int] = None, laude: Boolean = false) extends ExamResult {

  override def getKind: Kind.Value = kind

  override def getEvaluation: Option[Int] = eval

  override def cumLaude: Boolean = laude

  override def toString: String =  {
    var s: String = getKind.toString()
    // else "" is necessary, otherwise it prints "()" ???
    if(getKind == Kind.SUCCEEDED) s += "(" + getEvaluation.get + (if (cumLaude) "L" else "") + ")"
    s
  }
}

trait ExamResultFactory {
  def failed: ExamResult

  def retired: ExamResult

  def succeededCumLaude: ExamResult

  def succeeded(evaluation: Int): ExamResult
}

object ExamResultFactoryImpl extends ExamResultFactory {
  override def failed: ExamResult = ExamResultImpl(Kind.FAILED)

  override def retired: ExamResult = ExamResultImpl(Kind.RETIRED)

  override def succeededCumLaude: ExamResult = ExamResultImpl(Kind.SUCCEEDED, Some(30), laude = true)

  override def succeeded(evaluation: Int): ExamResult =
    if (evaluation < 18 || evaluation > 30) throw new IllegalArgumentException()
    else ExamResultImpl(Kind.SUCCEEDED, Some(evaluation))
}

trait ExamsManager {
  def createNewCall(call: String): Unit

  def addStudentResult(call: String, student: String, result: ExamResult): Unit

  def getAllStudentsFromCall(call: String): Set[String]

  def getEvaluationsMapFromCall(call: String): Map[String, Int]

  def getResultsMapFromStudent(student: String): Map[String, String]

  def getBestResultFromStudent(student: String): Option[Int]
}

case class ExamsManagerImpl() extends ExamsManager {

  var results: Map[String, Map[String, ExamResult]] = Map()

  private def checkArgument(condition: Boolean): Unit = {
    if (!condition) throw new IllegalArgumentException()
  }

  override def createNewCall(call: String): Unit = {
    checkArgument(!results.contains(call))
    results = results + (call -> Map.empty)
  }

  override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
    checkArgument(results.contains(call))
    val thisCall = results(call)
    checkArgument(!thisCall.contains(student))
    results = results + (call -> (thisCall + (student -> result)))
  }

  override def getAllStudentsFromCall(call: String): Set[String] = {
    checkArgument(results.contains(call))
    results(call).keySet
  }

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
    checkArgument(results.contains(call))
    results(call).collect { case (stud, res) if res.getKind == Kind.SUCCEEDED => stud -> res.getEvaluation.get }
  }

  override def getResultsMapFromStudent(student: String): Map[String, String] = {
    results.collect { case (call, resMap) if resMap.contains(student) => call -> resMap(student).toString }
  }

  override def getBestResultFromStudent(student: String): Option[Int] = {
    val studRes = results.collect { case (_, resMap) if resMap.contains(student) && resMap(student).getKind == Kind.SUCCEEDED => resMap(student).getEvaluation.get }
    if (studRes.isEmpty) None else Option(studRes.max)
  }

}


object ExamsManagerTest extends App {



}