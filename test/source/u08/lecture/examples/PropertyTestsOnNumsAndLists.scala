package lecture.examples

import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.{exists, forAll}
import org.scalatest.junit.JUnitRunner

// https://github.com/oscarrenalias/scalacheck-cookbook/blob/master/markdown/scalacheck-integration.md
@RunWith(classOf[ScalaCheckJUnitRunner])
class TestOnNumbers extends Properties("Numbers") {
  property("Sum is associative") = forAll{ (a: Int, b: Int, c: Int) =>
    (a+b)+c == a+(b+c)
  }
  property("Sum is commutative") = forAll { (a: Int, b: Int) =>
    a+b == b+a
  }
  property("Sum has zero as identity") = forAll { (a: Int) =>
    a + 0 == a && 0 + a == a
  }
  property("Diff is not associative") = forAll{ (a: Int, b: Int) =>
    exists{ c:Int => (a-b)-c != a-(b-c) }
  }
}

@RunWith(classOf[ScalaCheckJUnitRunner])
class TestOnLists extends Properties("Seqs") {
  def genericSizeProp[A:Arbitrary]: Prop = forAll{ (l1: Seq[A], l2: Seq[A]) =>
    (l1++l2).size == l1.size + l2.size
  }
  property("Size of concatenation") = Prop.all(
    genericSizeProp[Int], genericSizeProp[String], genericSizeProp[(Boolean,Double)])
  property("Reverse") = forAll { (l1: Seq[Int], l2: Seq[Int]) =>
    l1.reverse.reverse == l1
  }
}