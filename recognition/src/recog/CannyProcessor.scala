package recog

import org.opencv.core.Mat
import org.opencv.imgproc.Imgproc
import org.opencv.core.Core
import org.opencv.core.Scalar

class CannyProcessor extends ImageProcessor[Mat, Mat] {

  val threshold1 = new DoubleParameter("threshold1")
  threshold1.value = 200
  val threshold2 = new DoubleParameter("threshold2")
  threshold2.value = 300
  def name = "Canny" 
  def parameters: Seq[DoubleParameter] = Seq(threshold1, threshold2)
  
  def apply(mat: Mat): Mat = { 
    val rv = new Mat()
    Imgproc.Canny(mat, rv, threshold1.value, threshold2.value)
    rv
  }
  def draw(target: Mat, result: Mat) {
    Core.subtract(target, target, target, result)
    Core.add(target, new Scalar(255, 0, 0), target, result)
  }
}