package recog

import org.opencv.core.Mat
import scala.collection.Seq
import minefinder.grid.Grid
import org.opencv.core.Rect
import minefinder.field.Mark
import scala.language.implicitConversions
import minefinder.field.LazyField
import minefinder.field.RectangularCell
import minefinder.field.Cell
import minefinder.field.RectangularField
import minefinder.field.Matrix
import minefinder.field.Matrix

object CellProcessor {
  implicit def toRect(rectTuple: (Int, Int, Int, Int)) = {
    val (left: Int, top: Int, right: Int, bottom: Int) = rectTuple
    assert(left < right)
    assert(top < bottom)
    assert(left > 0)
    assert(top > 0)
    new Rect(left, top, right - left, bottom - top)
  }
//  implicit def rectToString(rect:Rect) = {
//    "%d, %d, %d, %d".format(rect.x, rect.y, rect.x+rect.width, rect.y+rect.height);
//  }
}

/**Handles cells within grid detected on an input */
abstract class CellProcessor[CellResult, GridData](
  val subProcessor: ImageProcessor[Mat, CellResult],
  val gridProcessor: ImageProcessor[Mat, GridData])
  extends ImageProcessor[Mat, (GridData, Option[RectangularField[CellResult]])] {
  import CellProcessor._;

  class ImageField(val grid: Grid, image: Mat) extends RectangularField[CellResult] {
    val matrix = new Matrix[RectangularCell[CellResult]](grid.xAxis.count, grid.yAxis.count) {
      def neighWrap(x:Int, y:Int) = neighboors(x, y)
      override def initializeValue(x0: Int, y0: Int) = {
        val rect: Rect = getRect(x0, y0)
        val region = new Mat(image, rect)
        new RectangularCell[CellResult] {
         override val mark = try {subProcessor(region)} catch { case e:Throwable => throw new RuntimeException("Failed to process cell %d, %d, %s".format(x, y, rect.toString), e)}
         override val x = x0
         override val y = y0
         override lazy val neighboors:Set[RectangularCell[CellResult]] = neighWrap(x, y)
        }
      }
    }
    def getRect(x: Int, y: Int) = {
      grid.getCell(x, y)
    }
    def getRect(cell: RectangularCell[CellResult]): (Int, Int, Int, Int) = {
      getRect(cell.x, cell.y)
    }
    override def cells: Set[RectangularCell[CellResult]] = matrix.contents
    override def get(x: Int,y: Int) = matrix(x, y) 

  }

  def name(): String = { "Cell" }

  def parameters(): Seq[DoubleParameter] = Seq.empty

  def getGrid(data:GridData): Option[Grid]

  override def apply(input: Mat): Result = {
    val gridData = gridProcessor(input)
    (gridData, getGrid(gridData).map( (grid:Grid) => new ImageField(grid, input)))
  }

  override def draw(target: Mat, result: Result) {
    def processField(field: ImageField) {
      for (cell <- field.cells) {
        val rect: Rect = field.getRect(cell)
        try {
          val region = new Mat(target, rect)
          subProcessor.draw(region, cell.mark)
        } catch {
          case e:Throwable => throw new IllegalArgumentException("Can draw region " + rect + " in image of size " + target.size, e); 
        }
      }
    }
    result._2.foreach(
      field => {
        processField(field.asInstanceOf[ImageField])
      } 
    )
    gridProcessor.draw(target, result._1)
  }
}
