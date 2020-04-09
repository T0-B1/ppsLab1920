package AlessandroMartignano.u04

import AlessandroMartignano.u04.Lists._
import AlessandroMartignano.u04.Optionals._
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()); // elementi già prodotti
    for (i <- 0 until 10) {
      pi.next(); // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test
  def testFromList() {
    val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
    val s = factory.fromList(l)
    assertEquals(Option.of(10), s.next());
    assertEquals(Option.of(20), s.next());
    assertEquals(Option.of(30), s.next());
    //assertEquals(Option.empty, s.next());
    assertEquals(l, s.allSoFar())
    val s2 = s.reversed()
    assertEquals(Option.of(30), s2.next());
    assertEquals(Option.of(20), s2.next());
    assertEquals(Option.of(10), s2.next());
    assertEquals(Option.empty, s2.next());
    assertEquals(List.reverse(l), s2.allSoFar())

  }

  @Test
  def testRandomBooleans(): Unit = {
    val s1 = factory.randomBooleans(10)
    val s2 = factory.randomBooleans(10)
    for(_ <- 0 to 10) {
      s1.next()
      s2.next()
    }
    assertNotEquals(s1.allSoFar(), s2.allSoFar())
    assertEquals(Option.empty, s1.next())
    assertEquals(Option.empty, s2.next())
  }
}