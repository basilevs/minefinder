package minefinder.grid

import math.{ min, max, abs }

class PeriodDetector(val falsePositive:Double, val falseNegative:Double, val rejectThreshold:Double)
extends Function[Set[Int], Option[Axis]] {
  val debug = true
  def apply(input: Set[Int]) = {
    findBest(input, calcScore(input, _))
  }
  def findBest(input: Set[Int], calcScore: Function1[Axis, Double]): Option[Axis] = {
    val sorted = input.toSeq.sortBy(a => a)
    val first: Int = sorted.head
    val last: Int = sorted.last
    val margin = (last - first)/20
    assert(sorted.head >= 0)
    assert(sorted.size > 10)
    println("Ticks: " + sorted.toString())

    val candidates: Seq[Axis] = for (
      start <- (margin).until(last/5);
      step <- 10.until(last / 10);
      count <- ((last - start) / step * 0.75).toInt.until((last - start) / step);
      stop = start + step * count
    ) yield {
      assert(start + step * count <= last)
      val rv = Axis(start, step, count)
      rv
    }
    //createAxis(start, step, count, last)

    if (candidates.isEmpty) {
      println("No Axis")
      None
    } else {
      val rv = candidates.maxBy(calcScore)
      println("Restored Axis(%d, %d, %d, %d)".format(rv.start, rv.step, rv.count, rv.stop), "score: " + calcScore(rv))
      Some(rv)
    }
  }

  def createAxis(start: Int, step: Int, count: Int, last: Int): Axis = {
    val rv = Axis(start, step, count)
    assert(rv.stop <= last)
    rv
  }

  val lineMask: Set[Int] = Set(-2, -1, 1, 2)

  def calcScore(input: Set[Int], candidate: Axis): Double = {
    var weight: Double = 0
    assert(candidate.stop <= input.max)
    val start = candidate.start % candidate.step
    val stop = input.max
    for (tick <- start.until(stop, candidate.step)) {
      val hit = lineMask.exists(d => input(d + tick))
      if (candidate.start <= tick && tick <= candidate.stop) {
        if (!hit)
          weight -= falseNegative
      } else {
        if (hit)
          weight -= falsePositive
      }
      if (weight < -rejectThreshold)
        return weight
    }
    for (x <- input) {
      val hit: Boolean = lineMask.exists(d => ((d + x - start) % candidate.step) == 0)
      if (!hit)
        weight -= falsePositive
      if (weight < -rejectThreshold)
        return weight
    }
    weight
  }

}