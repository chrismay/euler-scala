import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import Iterator.iterate

@RunWith(classOf[JUnitRunner])
class Ex3Suite extends FunSuite  with ShouldMatchers {
 
  def ex3(compound: Long): Long = {
   iterate(2L)(_+1).takeWhile(_ < compound/ 2).find(
        maybeFactor => (compound % maybeFactor == 0) ).map(
            factor=>ex3(compound/factor)).getOrElse(compound)
  }
  
  test("largest prime factor of 12 is 3")(ex3(12) should equal(3))
  test("largest prime factor of 25 is 5")(ex3(25) should equal(5))
  test("largest prime factor of 600851475143 is 6857")(ex3(600851475143L) should equal(6857))
  test("largest prime factor of 87625999 is 1471")(ex3(87625999) should equal(1471))
  test("largest prime factor of 59569 is 839")(ex3(59569) should equal(839))
}
