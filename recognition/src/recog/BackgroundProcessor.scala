package recog

import org.opencv.core.Mat
import org.opencv.imgproc.Imgproc
import org.opencv.core.Core._
import org.opencv.core.CvType
import org.opencv.core.Point
import org.opencv.core.Scalar
import org.opencv.core.Rect
import org.opencv.core.MatOfInt
import collection.JavaConversions._
import org.opencv.core.MatOfFloat
import org.opencv.core.Size
import org.opencv.imgproc.Moments

class BackgroundProcessor extends ImageProcessor[Mat, Mat] {
  val trueScalar = new Scalar(255)

  val floodStrength = new DoubleParameter("floodStrength", 20)
  val margin = new DoubleParameter("margin", 0.1)
  val iterations = new DoubleParameter("iterations", 0)
  val colorThreshold = new DoubleParameter("colorThreshold", 60)

  override def apply(input: Mat): Mat = {
    val mask = Mat.zeros(input.size, CvType.CV_8UC1)
    val hsv = new Mat
    Imgproc.cvtColor(input, hsv, Imgproc.COLOR_BGR2HSV)
    assert(mask.channels == 1)
    flood(input, mask)
    assert(mask.channels == 1)
    //seedMask(mask)
    removeSameColor(input, mask)
    grabCut(input, mask)
    
    mask
  }

  def seedMask(mask: Mat) = {
    for (seed <- getSeeds(mask.size)) {
      circle(mask, seed, 1, trueScalar, -1)
    }
  }

  private def blur(input: Mat) = {
    val size = math.min(input.height, input.width) / 10
    val output = new Mat(input.size, input.`type`())
    Imgproc.GaussianBlur(input, output, new Size(1, 1), 0)
    output
  }

  private def countEqual(input: Mat, color: Scalar) = {
    val comparison = new Mat
    compare(input, color, comparison, CMP_EQ)
    countNonZero(comparison)
  }

  private def grabCut(input: Mat, mask: Mat) = {
    assert(mask.channels == 1)
    val iterations = this.iterations.value.toInt
    if (iterations > 0) {
      val maskCount = countNonZero(mask)
      val total = mask.total()

      val foreground = new Scalar(Imgproc.GC_FGD)
      val probBackground = new Scalar(Imgproc.GC_PR_BGD)
      val probForeground = new Scalar(Imgproc.GC_PR_FGD)

      if (maskCount > 10 && total - maskCount > 10) {
        var grabMask = new Mat(mask.size, CvType.CV_8UC1)
        grabMask.setTo(probBackground)
        //rectangle(grabMask, new Point(0.3 * input.width, 0.15 * input.height), new Point(0.7 * input.width, 0.85 * input.height), probBackground)
        grabMask.setTo(foreground, mask)

        if (countEqual(grabMask, probBackground) > 10) {
          try {
            Imgproc.grabCut(input, grabMask, new Rect(), new Mat, new Mat, iterations, Imgproc.GC_INIT_WITH_MASK)
          } catch {
            case e: Throwable => throw new RuntimeException("Non-zero " + maskCount, e)
          }
          val result = mask.clone()
          compare(grabMask, probForeground, result, CMP_EQ)
          mask.setTo(trueScalar, result)
        }
      }
    }
  }

  private def removeSameColor(input: org.opencv.core.Mat, mask: org.opencv.core.Mat) = {
    val hsv = new Mat(input.size, CvType.CV_8UC3)
    val histSize = 32
    Imgproc.cvtColor(input, hsv, Imgproc.COLOR_BGR2HSV)
    val hist = new Mat
    val ranges = new MatOfFloat(0, 256, 0, 256)
    val channels = new MatOfInt(0, 1)
    Imgproc.calcHist(Seq(hsv), channels, mask, hist, new MatOfInt(histSize, histSize), ranges)
    normalize(hist, hist, 0, 255, NORM_MINMAX)
    val outputMask = new Mat
    Imgproc.calcBackProject(Seq(hsv), channels, hist, outputMask, ranges, 1)
    Imgproc.threshold(outputMask, outputMask, colorThreshold.value.toInt, 255, Imgproc.THRESH_BINARY)

    mask.setTo(Scalar.all(255), outputMask)
  }

  private def flood(input: Mat, mask: Mat) {
    val seeds = getSeeds(input.size)

    val bound = Scalar.all(floodStrength.value);
    val floodMask = Mat.zeros(input.rows + 2, input.cols + 2, CvType.CV_8UC1)
    val zero = Scalar.all(0);
    val one = Scalar.all(255);
    val rect = new Rect(1, 1, input.width, input.height)
    for (seed <- seeds) {
      Imgproc.floodFill(input, floodMask, seed, Scalar.all(0), null, bound, bound, Imgproc.FLOODFILL_MASK_ONLY)
    }
    mask.setTo(one, new Mat(floodMask, rect))
  }

  private def seeds = {
    val margin: Double = this.margin.value
    Seq[(Double, Double)](
      (0D, 0D),
      (1, 0),
      (1, 1),
      (0, 1),
      (0, 0.5),
      (0.5, 0),
      (1, 0.5),
      (0.5, 1))
  }

  private def getSeeds(input: Size): Seq[org.opencv.core.Point] = {
    val margin: Int = (input.height * this.margin.value).toInt
    val seeds = Seq(
      new Point(0, 0),
      new Point(input.width - 1, input.height - 1),
      new Point(0, input.height - 1),
      new Point(input.width - 1, 0),
      new Point(input.width / 2, input.height - 1),
      new Point(input.width / 2, 0),
      new Point(0, input.height / 2),
      new Point(2, input.height / 2),
      new Point(4, input.height / 2),
      new Point(input.width - 1, input.height / 2),
      new Point(input.width - 2, input.height / 2),
      new Point(margin, margin),
      new Point(input.width - margin, margin),
      new Point(input.width - margin, input.height / 2),
      new Point(input.width - margin, input.width - margin),
      new Point(margin, input.height - margin),
      new Point(input.width - margin, margin),
      new Point(margin, input.height / 2),
      new Point(margin, input.height - 2 * margin))
    seeds
  }

  override def draw(target: Mat, result: Mat): Unit = {
    target.setTo(Scalar.all(0), result)
  }

  override def name: String = "Background"

  override def parameters: Seq[recog.DoubleParameter] = Seq(floodStrength, colorThreshold, margin, iterations)

}