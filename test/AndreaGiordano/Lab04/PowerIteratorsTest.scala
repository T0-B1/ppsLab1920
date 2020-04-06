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
    val pi2 = factory.randomBooleans(10)
    for (i <- 0 until 10){
      print("i"+i+" :"+pi2.next())
    }
    println()
    println(pi2.next())
  }
}