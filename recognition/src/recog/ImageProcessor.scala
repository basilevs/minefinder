package recog

import org.opencv.core.Mat

trait ImageProcessor[-T, R] extends Function2[T, (ImageProcessor[_, _],  Any) => Unit, R] {
  type Result = R
  type Hook = (ImageProcessor[_, _],  Any) => Unit
  def name: String
  def parameters: Seq[DoubleParameter]
  def draw(target: Mat, result: Any)
  def apply(input:T, hook:Hook): R
}