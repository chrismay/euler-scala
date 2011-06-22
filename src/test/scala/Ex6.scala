import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.math.Ordering
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex6Suite  extends FunSuite with ShouldMatchers {

  class SquareableLong(self:Long){
    def ** = self * self
  }
  implicit def i2sq(i:Int)= new SquareableLong(i)
  
  
  val sumOfSquares = (max:Int) =>{
    (1 to max).map(_ **).reduceLeft(_+_)   
  }
  val squareOfSum = (max:Int)=>{
    (1 to max).reduceLeft(_+_) **
  }
  test("Sum of squares to 10 is 385")( sumOfSquares(10) should equal(385l))
  test("Square of sums to 10 is 3025")( squareOfSum(10) should equal(3025l))
  test("Sum of squares to 100 is 338350")( sumOfSquares(100) should equal(338350l))
  test("Square of sums to 100 is 25502500")( squareOfSum(100) should equal(25502500l))
  

}