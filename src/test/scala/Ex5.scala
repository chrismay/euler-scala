import org.scalatest.matchers.ShouldMatchers
import Iterator.iterate

import org.scalatest.FunSuite
import scala.math.Ordering
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class MergeableMap[A, B](self: Map[A, B]) {
  def merge(m1: Map[A, B], merger: (B, B) => B): Map[A, B] = {
    (Map[A, B]() /: (for (m <- List(m1, self); kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> merger(a(kv._1), kv._2) else kv)
    }
  }
}


@RunWith(classOf[JUnitRunner])
class Ex5Suite extends FunSuite with ShouldMatchers {
  implicit def map2mergeableMap[A,B](m:Map[A,B]):MergeableMap[A,B] = new MergeableMap(m)
  
  /**
   *  Map of prime factor->power for a given value
   */
  def primeFactors(x: Int): Map[Int, Int] = {
    factorIterator(x).foldLeft(Map.empty[Int, Int])((factors, f) => {
      factors + ((f._1, 1 + factors.getOrElse(f._1, 0)))
    })
  }

  /**
   * Given an integer, emit pairs of it's prime factors + quotients, repeatedly
   * dividing the quotient by the next lowest prime factor
   *
   */
  def factorIterator(x: Int): Iterator[Tuple2[Int, Int]] = {
    iterate((1, x))(p => (lowestFactor(p._2), p._2 / lowestFactor(p._2))).drop(1).takeWhile(t => (t._1 > 1))
  }

  def lowestFactor(x: Int): Int = {
    (2 to x/2).find(y => ((x % y) == 0)).getOrElse(x)
  }

  def addFactors(m: Map[Int, Int], y: Int): Map[Int, Int] = {
    m.merge(primeFactors(y), (i, j) => (if (i > j) i else j))
  }

  def lowestCommonMultiple(x: Int): Int = {
    val allPrimeFactors = ((1 to x).foldLeft(Map.empty[Int, Int])(addFactors))
    allPrimeFactors.foldLeft(1d)((acc: Double, kv) => (acc * Math.pow(kv._1, kv._2))).toInt

  }
  test("lowest common mulitple of 20 is 232792560")(lowestCommonMultiple(20) should equal(232792560))

  test("lowest common mulitple of 10 is 2520")(lowestCommonMultiple(10) should equal(2520))
}