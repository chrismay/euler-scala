import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Ex1Suite extends FunSuite with ShouldMatchers{
   def multipleOf3Or5(i: Int): Int = {
    if (i % 3 == 0 || i % 5 == 0) 1 else 0
  }
  def fizBuzSum(upTo: Int): Int = {
    (1 until upTo).foldLeft(0)((tot, i) => { tot + (i * multipleOf3Or5(i)) })
  }
  
  test("sum from 1 to 10 is 23")(fizBuzSum(10) should equal(23))
  test("sum from 1 to 1000 is 23")(fizBuzSum(1000) should equal(233168))

}
