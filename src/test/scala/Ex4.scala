import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.math.Ordering
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex4Suite extends FunSuite with ShouldMatchers {

  def isPalindrome(num:Int):Boolean ={
    num.toString().reverse.toInt == num
  }
   
  def ex4(max:Int) :Int = {
    (for (x<-(1 to max); y<-(1 to max)) yield x*y).sortWith(_>_).find(isPalindrome).getOrElse(1)
  }
  
  test("Largest product-of-1-digit-numbers palindrome is 9")(ex4(9) should equal(9))
  test("Largest product-of-2-digit-numbers palindrome is 9009")(ex4(99) should equal(9009))
  test("Largest product-of-3-digit-numbers palindrome is 906609")(ex4(999) should equal(906609))

  test("101 is palindrome")(isPalindrome(101) should be(true))
  test("123 is not a palindrome")(isPalindrome(123) should be(false))
}