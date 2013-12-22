package recog

import org.opencv.core.Mat
import scala.collection.Seq
import minefinder.grid.Grid
import org.opencv.core.Rect
import minefider.opencv.RecognizingField
import minefinder.field.Mark
import scala.language.implicitConversions


object CellProcessor {
    implicit def toRect(rectTuple:(Int, Int, Int, Int)) = {
        val (left:Int, top:Int, right:Int, bottom:Int) = rectTuple
        new Rect(left, top, right, bottom)
    }
}

class CellProcessor(
    val subProcessor: ImageProcessor[Mat, Option[Mark]],
    val gridProcessor: ImageProcessor[Mat, Option[Grid]])
    extends ImageProcessor[Mat, Option[RecognizingField]] {
    import CellProcessor._;

    def name(): String = { "Cell" }

    def parameters(): Seq[DoubleParameter] = { null }

    override def apply(input: Mat): Result = {
        val gridOption = gridProcessor(input)
        def recognizeArea(left:Int, top:Int, right:Int, bottom:Int):Option[Mark] = {
            val region = new Mat(input, (left, top, right, bottom))
            subProcessor(region)
        }
        gridOption.map(new RecognizingField(_, recognizeArea))
    }
    
    override def draw(target: Mat, result: Any) {
       val fieldOption = result.asInstanceOf[Option[RecognizingField]]
       def processField(field:RecognizingField) {
           for (cell <- field.cells) {
               val region = new Mat(target, field.getRect(cell))
               subProcessor.draw(region, cell.mark)
           }
       }
    }
}
