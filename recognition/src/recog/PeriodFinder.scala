package recog
import collection.mutable.Map
import math.min

class PeriodFinder extends Function[Set[Int], (Int, Int, Int)] {
  type IntervalDescriptor = (Int, Int, Int)
  def apply(input: Set[Int]) = {
    val sorted = input.toList.sortBy(a => a)
    assert(sorted.head >= 0)
    val candidates = for {
      startIdx <- 0 until sorted.size
      stepIdx <- 0 until min(10, sorted.size - startIdx)
      count <- 10 until (sorted.last / sorted.size)
    } yield {
      (sorted(startIdx), sorted(startIdx + stepIdx) - sorted(startIdx), count)
    }
    
    candidates.maxBy(calcScore(input, _))
  }
  
  

  def calcScore(input: Set[Int], candidate: IntervalDescriptor) = {
    var sum: Int = 0
    for (x <- candidate._1.until(candidate._3 * candidate._2 + candidate._1, candidate._2)) {
      if (input.contains(x))
        sum += 1
    }
    sum / candidate._3
  }

}