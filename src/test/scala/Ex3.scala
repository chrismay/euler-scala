import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Ex3Suite extends FunSuite with ShouldMatchers {
  lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))

 def pickFactor(i:Int):Int={
      println("getting factor for " + i);
       naturals.takeWhile(_<i/2).foldLeft(i)((num,maybeFactor)=>(if (num % maybeFactor ==0)  num/maybeFactor else num))
  }
  def ex3(i:Int):Int={
    var start = pickFactor(i)
    var next = pickFactor(i/start);
    if (start == next  ) start  else ex3(next)
  }
  test("naturals from 1 to 5 look right")(naturals.take(5).mkString(",") should equal("1,2,3,4,5"))
  
  test("largest prime factor of 12 is 3")(ex3(12) should equal(3))
}