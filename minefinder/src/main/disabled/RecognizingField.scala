package minefider.opencv

import minefinder.grid.Grid
import minefinder.field.LazyField
import org.opencv.core.Mat
import minefinder.field.Mark
import collection.mutable
import minefinder.field.Cell
import minefinder.events.EventSource
import minefinder.events.EventBus
import minefinder.field.RectangularField
import minefinder.field.RectangularCell

/**
 * Handles recognition of items within grid
 *
 */
class RecognizingField[T](val grid: Grid, val image: (Int, Int, Int, Int) => T) extends RectangularField[T] {
    private val field = new LazyField[Option[T]](grid.xAxis.count, grid.yAxis.count) {
        override def updateValue(cell: RectangularCell[Option[T]]): Option[T] = {
            Some(cell.mark.getOrElse{recognize(cell)})
        }
        override def defaultValue = None
    }
    private val eventBus = new EventBus[Cell[Option[T]]]()

    def recognize(cell: RectangularCell[Option[T]]): T = {
        val rect = getRect(cell)
        val result: T = image.tupled(rect)
        result
    }
    def cells: Set[RectangularCell[T]] = field.cells
    def reactions: EventSource[Cell[Option[T]]] = eventBus

    def getRect(cell: RectangularCell[Option[T]]): (Int, Int, Int, Int) = {
        assert(field.cells contains cell)
        grid.getCell(cell.x, cell.y)
    }

    def update(cell: RectangularCell[Option[T]]) {
    }
}