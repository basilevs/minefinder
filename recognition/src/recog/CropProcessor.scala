package recog

import org.opencv.core.Mat
import org.opencv.core.Rect
import org.opencv.core.Size

class CropProcessor[T](val next:ImageProcessor[Mat, T]) extends ImageProcessor[Mat, T] {
  override def name: String = "Crop"
  val side = new DoubleParameter("side", 0.05)
  val top = new DoubleParameter("top", 0.1)
  val bottom = new DoubleParameter("bottom", 0.09)
  override def parameters: Seq[DoubleParameter] = Seq(side, top, bottom)
  override def draw(target: Mat, result:T) = {
    next.draw(crop(target), result) 
  }
  
  private def getBounds(size:Size): Rect = {
    val sideCrop:Int = (size.width * side.value).toInt
    val topCrop:Int = (size.height * top.value).toInt
    new Rect(sideCrop, topCrop, (size.width - 2 * sideCrop).toInt, (size.height - topCrop - size.height * bottom.value).toInt)
  }
  private def crop(input:Mat) = new Mat(input, getBounds(input.size))
  override def apply(input:Mat): T = next.apply(crop(input)) 
}