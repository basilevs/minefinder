package recog

import org.opencv.core.Mat
import scala.collection.Seq
import minefinder.grid.Grid
import org.opencv.core.Rect

class CellProcessor[T, R](
  val subProcessor: ImageProcessor[Mat, R],
  val gridProcessor: ImageProcessor[Mat, Option[Grid]])
  extends ImageProcessor[Mat, Seq[Seq[R]]] {

  def name(): String = { "Cell" }

  def parameters(): Seq[DoubleParameter] = { null }

  override def apply(input: Mat) = {
    val grid = gridProcessor(input)
    val seqOption = grid.map {
      case grid => {
        grid.forEachCell {
          case (x, y, left, top, right, bottom) => {
            val rect = new Rect(left, top, right - left, bottom - top)
            subProcessor(new Mat(input, rect))
          }
        }
      }
    }
    seqOption.getOrElse(Seq.empty)
  }

  def draw(target: Mat, result: Any): Unit = {
	  
  }

}