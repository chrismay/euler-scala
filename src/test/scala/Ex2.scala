import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex2Suite extends FunSuite with ShouldMatchers {
 
  lazy val fib: Stream[Int] = Stream.cons(0, Stream.cons(1, fib.zip(fib.tail).map(p => p._1 + p._2)))

  def ex2(max: Int): Int = {
    fib.takeWhile(_ < max).filter(_%2==0).foldLeft(0)(_+_)
  }
  
  test("fib actually generates the fibonaci sequence")(fib.take(5).mkString(",") should equal("0,1,1,2,3"))
  test("sum of first 10 even members is 44")(ex2(89) should equal(44))
  test("sum of even members below 4000000 is 44")(ex2(400000) should equal(257114))

}