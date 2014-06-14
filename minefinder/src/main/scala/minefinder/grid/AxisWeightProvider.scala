package minefinder.grid

import math.{min, abs}

object AxisWeightProvider {
  val lineMask: Set[Int] = Set(-2, -1, 0, 1, 2)
}
class AxisWeightProvider(val input: Set[Int], val falsePositive: Double, val falseNegative: Double, val error: Double) {
  import AxisWeightProvider._;
  val minCount = 9
  private val sorted = input.toSeq.sorted
  val first: Int = sorted.head
  val last: Int = sorted.last
  assert (last > first)
  assert (error > 0)
  assert(falsePositive >= 0)
  assert(falseNegative >= 0)
  val maxStart = if (sorted.size > 5) sorted(4) else first + (last - first)/5
  println("Ticks: " + sorted.toString()) 
  
  def findBestWithWidth(width: Int, rejectThreshold: Double): Option[(Axis, Double)] = {
    var bestScore = -rejectThreshold
    var bestAxis:Option[Axis] = None
    for (
      start <- first to maxStart;
      count <- minCount to ((last - start) / width + 1);
      axis = Axis(start, width, count);
      score = calcScore(axis, -bestScore)
      if (score > bestScore)
    ) {
      bestScore = score
      bestAxis = Some(axis)
    }

    bestAxis.map((_, bestScore))
  }

  def calcScore(candidate: Axis, rejectThreshold: Double): Double = {
    var score: Double = 0
    val step = candidate.step
    val error:Int = (this.error * step).toInt
    def isTick(x:Int) = {
      val inRange = x > candidate.start - error && x < candidate.stop + error
      val mod = (x - candidate.start) % step 
      val isTick = mod < error || mod > step - error
      inRange && isTick
    }

    var i = 1
    for (tick <- candidate.ticks) {
      while (i+1 < sorted.size && sorted(i) <= tick) i+=1
      def isClose(x:Int) = abs(x - tick) < error
      val hit = isClose(sorted(i - 1)) || isClose(sorted(i))
      if (!hit)
        score -= falsePositive
      if (score < -rejectThreshold)
        return score
    }
  

    for (x <- sorted) {
     if (!isTick(x)) {
        score -= falseNegative
        if (score < -rejectThreshold)
          return score
      }
    }
    score
  }

}