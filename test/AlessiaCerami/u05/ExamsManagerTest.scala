package AlessiaCerami.u05

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ExamsManagerTest {
  // This could be a singleton
  private val erf = ExamResultFactoryImpl()
  // This shouldn't
  private val em = ExamsManagerImpl

  // verifica base di ExamResultFactory
  /*
  @Test
  def testExamResultsBasicBehaviour() { // esame fallito, non c'è voto
    assertEquals(erf.failed.getKind, Kind.FAILED)
    assertFalse(erf.failed.getEvaluation.nonEmpty)
    assertFalse(erf.failed.cumLaude)
    assertEquals(erf.failed.toString, "FAILED")
    // lo studente si è ritirato, non c'è voto
    assertEquals(erf.retired.getKind, Kind.RETIRED)
    assertFalse(erf.retired.getEvaluation.nonEmpty)
    assertFalse(erf.retired.cumLaude)
    assertEquals(erf.retired.toString, "RETIRED")
    // 30L
    assertEquals(erf.succeededCumLaude.getKind, Kind.SUCCEEDED)
    assertEquals(erf.succeededCumLaude.getEvaluation, Option(30))
    assertTrue(erf.succeededCumLaude.cumLaude)
    assertEquals(erf.succeededCumLaude.toString, "SUCCEEDED(30L)")
    // esame superato, ma non con lode
    assertEquals(erf.succeeded(28).getKind, Kind.SUCCEEDED)
    assertEquals(erf.succeeded(28).getEvaluation, Option(28))
    assertFalse(erf.succeeded(28).cumLaude)
    assertEquals(erf.succeeded(28).toString, "SUCCEEDED(28)")
  }
   */

  // verifica eccezione in ExamResultFactory
  @Test
  def optionalTestEvaluationCantBeGreaterThan30(): Unit = {
    try {
      erf.succeeded(32)
    }
    catch {
      case e: IllegalArgumentException => ()
      case _: Throwable => fail()
    }
  }

  @Test
  def optionalTestEvaluationCantBeSmallerThan18(): Unit = {
    try {
      erf.succeeded(17)
    }
    catch {
      case e: IllegalArgumentException => ()
      case _: Throwable => fail()
    }
  }

  // metodo di creazione di una situazione di risultati in 3 appelli
  private def prepareExams() {
    em.createNewCall("gennaio")
    em.createNewCall("febbraio")
    em.createNewCall("marzo")
    em.addStudentResult("gennaio", "rossi", erf.failed) // rossi -> fallito

    em.addStudentResult("gennaio", "bianchi", erf.retired) // bianchi -> ritirato

    em.addStudentResult("gennaio", "verdi", erf.succeeded(28)) // verdi -> 28

    em.addStudentResult("gennaio", "neri", erf.succeededCumLaude) // neri -> 30L

    em.addStudentResult("febbraio", "rossi", erf.failed) // etc..

    em.addStudentResult("febbraio", "bianchi", erf.succeeded(20))
    em.addStudentResult("febbraio", "verdi", erf.succeeded(30))
    em.addStudentResult("marzo", "rossi", erf.succeeded(25))
    em.addStudentResult("marzo", "bianchi", erf.succeeded(25))
    em.addStudentResult("marzo", "viola", erf.failed)
  }

  // verifica base della parte obbligatoria di ExamManager
  @Test
  def testExamsManagement() {
    this.prepareExams()
    // partecipanti agli appelli di gennaio e marzo
    assertEquals(em.getAllStudentsFromCall("gennaio"), Set("rossi", "bianchi", "verdi", "neri"))
    assertEquals(em.getAllStudentsFromCall("marzo"), Set("rossi", "bianchi", "viola"))
    // promossi di gennaio con voto
    assertEquals(em.getEvaluationsMapFromCall("gennaio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("verdi") , 28)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("neri"), 30)
    // promossi di febbraio con voto
    assertEquals(em.getEvaluationsMapFromCall("febbraio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("bianchi"), 20)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("verdi"), 30)
    // tutti i risultati di rossi (attenzione ai toString!!)
    assertEquals(em.getResultsMapFromStudent("rossi").size, 3)
    assertEquals(em.getResultsMapFromStudent("rossi")("gennaio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("febbraio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("marzo"), "SUCCEEDED(25)")
    // tutti i risultati di bianchi
    assertEquals(em.getResultsMapFromStudent("bianchi").size, 3)
    assertEquals(em.getResultsMapFromStudent("bianchi")("gennaio"), "RETIRED")
    assertEquals(em.getResultsMapFromStudent("bianchi")("febbraio"), "SUCCEEDED(20)")
    assertEquals(em.getResultsMapFromStudent("bianchi")("marzo"), "SUCCEEDED(25)")
    // tutti i risultati di neri
    assertEquals(em.getResultsMapFromStudent("neri").size, 1)
    assertEquals(em.getResultsMapFromStudent("neri")("gennaio"), "SUCCEEDED(30L)")
  }

  // verifica del metodo ExamManager.getBestResultFromStudent
  @Test
  def optionalTestExamsManagement() {
    this.prepareExams()
    // miglior voto acquisito da ogni studente, o vuoto..
    assertEquals(em.getBestResultFromStudent("rossi"), Option(25))
    assertEquals(em.getBestResultFromStudent("bianchi"), Option(25))
    assertEquals(em.getBestResultFromStudent("neri"), Option(30))
    assertEquals(em.getBestResultFromStudent("viola"), None)
  }

  @Test
  def optionalTestCantCreateACallTwice() {
    try {
      this.prepareExams()
      em.createNewCall("marzo")
    }
    catch {
      case e: IllegalArgumentException => ()
      case _: Throwable => fail()
    }
  }

  @Test
  def optionalTestCantRegisterAnEvaluationTwice() {
    try {
      this.prepareExams()
      em.addStudentResult("gennaio", "verdi", erf.failed)
    }
    catch {
      case e: IllegalArgumentException => ()
      case _: Throwable => fail()
    }
  }

}
