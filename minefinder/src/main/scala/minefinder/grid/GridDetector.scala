package minefinder.grid;
import math.abs
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps;
import java.util.concurrent.TimeoutException

/**
 *  Detects a grid in a set of lines.
 */
class GridDetector(val falsePositive: Double, val falseNegative: Double, val rejectThreshold: Double, val error:Double)
  extends Function1[Seq[Array[Double]], Option[Grid]] {

  def close(a: Double, b: Double) = abs(a - b) < 0.02

  def lineDirection(input: Array[Double]): Double = {
    assert(input.length == 4)
    math.atan2(input(1) - input(3), input(0) - input(2))
  }

  def isVertical(input: Array[Double]): Boolean = {
    val direction = lineDirection(input)
    close(math.abs(direction), math.Pi / 2)
  }

  def isHorizontal(input: Array[Double]): Boolean = {
    val direction = lineDirection(input)
    close(direction % math.Pi, 0) || close(direction % math.Pi, math.Pi)
  }

  def average(a: Double, b: Double): Double = (a + b) / 2D
  def verticalCoordinate(line: Array[Double]): Double = average(line(1), line(3))
  def horizontalCoordinate(line: Array[Double]): Double = average(line(0), line(2))

  def length(axisProvider: AxisWeightProvider) = axisProvider.last - axisProvider.first

  class WidthProcessor(val hCoordinates: Set[Int], val vCoordinates: Set[Int]) {
    var threshold = rejectThreshold
    println("Horizontal: ") 
    private val hProvider = new AxisWeightProvider(hCoordinates, falsePositive, falseNegative, error)
    println("Vertical: ") 
    private val vProvider = new AxisWeightProvider(vCoordinates, falsePositive, falseNegative, error)
    private val sizes = Seq(length(hProvider), length(vProvider))

    val minWidth = math.max(sizes.min / 100, 10)
    val maxWidth = sizes.max / 5
    def processWidth(width: Int): Option[(Axis, Axis, Double)] = {
      val horizontalOption = hProvider.findBestWithWidth(width, threshold)
      horizontalOption.flatMap(horizontal => {
        println("Restored horizontal " + horizontal._1 + " score: " + horizontal._2)

        val verticalOption = vProvider.findBestWithWidth(width, threshold)
        verticalOption.map(vertical => {
          println("Restored vertical " + vertical._1 + " score: " + vertical._2)
          threshold = math.min(-math.min(horizontal._2, vertical._2), threshold)
          (horizontal._1, vertical._1, horizontal._2 + vertical._2)
        })
      })
    }

  }

  /**
   * Detects a grid in set of lines
   *  @param input - a seuqence of lines as array in format (x1, y1, x2, y2)
   */
  def apply(input: Seq[Array[Double]]): Option[Grid] = {
    val hCoordinates = input.filter(isVertical).map(horizontalCoordinate).map(_.toInt)
    val vCoordinates = input.filter(isHorizontal).map(verticalCoordinate).map(_.toInt)
    if (vCoordinates.isEmpty || hCoordinates.isEmpty) {
      None
    } else {
      try {
        val widthProcessor = new WidthProcessor(hCoordinates.toSet, vCoordinates.toSet)

        val candidates = (widthProcessor.minWidth until widthProcessor.maxWidth).flatMap(widthProcessor.processWidth)
        if (candidates.isEmpty) {
          None
        } else {
          val bestGrid = candidates.maxBy(_._3)
          Some(Grid(bestGrid._1, bestGrid._2))
        }
      } catch {
        case e: TimeoutException => {
          println("Timeout")
          None
        }
      } finally {
        println("Grid detection complete")
      }
    }
  }

}