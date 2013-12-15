package minefinder.grid
import math.abs

/** 
 *  Detects a grid in a set of lines.
 */
class GridDetector(val falsePositive: Double, val falseNegative: Double, val rejectThreshold: Double)
  extends Function1[Seq[Array[Double]], Option[Grid]] {
  val periodDetector = new PeriodDetector(falsePositive, falseNegative, rejectThreshold)

  def close(a: Double, b: Double) = abs(a - b) < 2

  def isVertical(input: Array[Double]): Boolean = {
    assert(input.length == 4)
    val rv = close(input(0), input(2)) && !close(input(1), input(3))
    rv
  }

  def isHorizontal(input: Array[Double]): Boolean = {
    assert(input.length == 4)
    !close(input(0), input(2)) && close(input(1), input(3))
  }

  /**
   * Detects a grid in set of lines
   *  @param input - a seuqence of lines as array in format (x1, y1, x2, y2)
   */
  def apply(input: Seq[Array[Double]]): Option[Grid] = {
    val horizontalCoordinates = input.filter(isVertical).map(_(0).toInt)
    val verticalCoordinates = input.filter(isHorizontal).map(_(1).toInt)
    if (verticalCoordinates.isEmpty || horizontalCoordinates.isEmpty) {
      None
    } else {
      val horizontal = periodDetector(horizontalCoordinates.toSet[Int])
      val vertical = periodDetector(verticalCoordinates.toSet[Int])
      if (horizontal.isEmpty || vertical.isEmpty)
        None
      else
        Some(Grid(horizontal.get, vertical.get))
    }
  }

}