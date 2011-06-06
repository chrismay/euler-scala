import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Ex3Suite extends FunSuite with ShouldMatchers {
  lazy val factors: Stream[Long] = Stream.cons(2, factors.map(_ + 1))

  def lowestFactor(i: Long): Option[Long] = {
    factors.takeWhile(_ < i / 2).find((j: Long) => (i % j == 0))
  }
  def ex3(i: Long): Long = {
   val f = lowestFactor(i)
   f match {
      case None => i
      case _ => ex3(i/f.get)		
    }
  }
  test("factors from 1 to 5 look right")(factors.take(5).mkString(",") should equal("2,3,4,5,6"))

  test("largest prime factor of 12 is 3")(ex3(12) should equal(3))
  test("largest prime factor of 25 is 5")(ex3(25) should equal(5))
  test("largest prime factor of 600851475143 is 6857")(ex3(600851475143L) should equal(6857))
  test("largest prime factor of 87625999 is 1471")(ex3(87625999) should equal(1471))
  test("largest prime factor of 59569 is 839")(ex3(59569) should equal(839))
} 