package source.u02

object CaseMatch extends App {

  // case-based function (a partial function in this case)
  val f: Int => String = {
    case n if n>0 => "pos"
    case 0 => "last"
  }
  println(f(1)) // pos
  println(f(0)) // last
  // f(-1) raises a scala.MatchError

  // in-site application of a case-based function with 'match'
  val res = 5 match {
    case n if n>0 => "pos"
    case 0 => "last"
    case _ => "neg" // default case, makes it a total function
  }
  println(res) // pos

}
