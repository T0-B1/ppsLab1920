package AndreaGiordano.Lab04

import AndreaGiordano.Lab04.Lists._
import AndreaGiordano.Lab04.Optionals._
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
    val pi3 = factory.incremental(0,_+3)
    for (_ <- 0 until 100){
      pi3.next()
    }
    assertEquals(Option.of(300), pi3.next())
  }

  @Test
  def testFromList() {
    import List._
    val list:List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Nil())))))))
    val pi4 = factory.fromList(list)
    assertEquals(Option.of(1), pi4.next())
    assertEquals(Option.of(2), pi4.next())
    pi4.next();pi4.next();pi4.next()
    assertEquals(Option.of(6), pi4.next())
    pi4.next()
    val pi6 = pi4.reversed()
    assertEquals(Option.empty, pi4.next())
    assertEquals(Option.of(7), pi6.next())
    assertEquals(Option.of(6), pi6.next())
    pi6.next();pi6.next();pi6.next()
    assertEquals(Option.of(2), pi6.next())
    assertEquals(Option.of(1), pi6.next())
    assertEquals(Option.empty, pi6.next())
  }

  @Test
  def testRandomBoolean() {
    val pi2 = factory.randomBooleans(100)
    val pi5 = factory.randomBooleans(100)
    for (_ <- 0 until 100){
      pi2.next()
      pi5.next()
    }
    assertNotEquals(pi2.allSoFar(), pi5.allSoFar())
    assertEquals(Option.empty, pi2.next())
    assertEquals(Option.empty, pi5.next())
  }
}