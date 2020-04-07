package AlessiaCerami.u04

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import AlessiaCerami.u04.Optionals._
import AlessiaCerami.u04.Lists._
import AlessiaCerami.u04.Lists.List._

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
  def testFromList(): Unit ={
    val list:List[Int] = Cons(1, Cons(2, Cons(5, nil)))
    val pi3 = factory.fromList(list)
    assertEquals(Option.of(1), pi3.next());
    assertEquals(Option.of(2), pi3.next());
    assertEquals(Option.of(5), pi3.next());
  }

  @Test
  def testRandomBoolean(): Unit ={
    val pi = factory.randomBooleans(4); // pi produce 5,7,9,11,13,...
    val pi2 = factory.randomBooleans(4); // pi produce 5,7,9,11,13,...
    assertNotEquals(pi,pi2)

  }

}