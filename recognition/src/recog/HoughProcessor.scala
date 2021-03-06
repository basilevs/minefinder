package recog

import org.opencv.core.Mat
import scala.collection.Seq
import org.opencv.imgproc.Imgproc
import org.opencv.core.Core
import org.opencv.core.MatOfByte
import org.opencv.core.CvType
import org.opencv.core.Point
import org.opencv.core.Scalar
import math.{min, abs}

class HoughProcessor extends ImageProcessor[Mat, Seq[Array[Double]]] {

  def name(): String = { "Houg" }

  def process(mat: Mat): Mat = { null }

  val rho = new DoubleParameter("rho")
  val theta = new DoubleParameter("theta")
  val threshold = new DoubleParameter("houghThreshold")
  val minLength = new DoubleParameter("minLength")
  val maxGap = new DoubleParameter("maxGap")
  rho.value = 2
  theta.value = 0.01
  threshold.value = 200
  minLength.value = 0.6
  maxGap.value = 0.2

  def lineDirection(input: Array[Double]): Double = {
    assert(input.length == 4)
    math.atan2(input(1) - input(3), input(0) - input(2))
  }

  def parameters(): Seq[DoubleParameter] = Seq(rho, theta, threshold, minLength, maxGap)
  def apply(mat: Mat): Seq[Array[Double]] = {
    val lines = new Mat
    assert(lines.`type`() == CvType.CV_8U)
    val size = min(mat.width, mat.height)
    Imgproc.HoughLinesP(mat, lines, rho.value, theta.value, threshold.value.toInt, minLength.value * size, maxGap.value * size)

    val linesAsArrays = for (i <- 0 until lines.cols())
      yield lines.get(0, i)
    linesAsArrays.filter(line => (abs(lineDirection(line)) % (math.Pi/2)) < 0.02 )
  }
  def draw(target: Mat, result: Seq[Array[Double]]) {
    val lines = result.asInstanceOf[Seq[Array[Double]]]
    for (line <- lines) {
      Core.line(target, new Point(line(0), line(1)), new Point(line(2), line(3)), new Scalar(0, 255, 0), 1)
    }
  }
}