import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex15 extends FunSuite with ShouldMatchers {
  val tstart = System.currentTimeMillis

  val finish = new Pair(14, 14)

  val start = new King((0, 0), List((0, 0)))
  val c = countPathsFrom(start)
  val t = System.currentTimeMillis - tstart

  println(c + " in " + t + "ms")

  case class King(pos: Pair[Int, Int], trail: List[Pair[Int, Int]]) {
    def getNextValidStates(): List[King] = {
      List((pos._1 + 1, pos._2), (pos._1, pos._2 + 1)).
      	filter(c=>trail.length < finish._1 ||  
      	      ((c._1 <= finish._1) && (c._2 <= finish._2))).
      	map(c => new King(c, trail ::: List(c)))
    }
  }

  def countPathsFrom(k: King): Int = {
    if (k.pos == finish) 1 else  k.getNextValidStates().map(countPathsFrom).reduce(_ + _)
    
  }


}