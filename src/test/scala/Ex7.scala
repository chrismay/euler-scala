import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.math.Ordering
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex7Suite extends FunSuite with ShouldMatchers {

  def ints(start: Int): Stream[Int] = {
    Stream.cons(start, ints(start + 1))
  }
  
  def primes(nums: Stream[Int]): Stream[Int] = {
    Stream.cons(nums.head, primes((nums.tail) filter (x => x % nums.head != 0)))
  }

  test("sixth prime is 13")(primes(ints(2)).drop(5).head should equal(13))
  test("1001th prime is 7927")(primes(ints(2)).drop(1000).head should equal(7927))

}