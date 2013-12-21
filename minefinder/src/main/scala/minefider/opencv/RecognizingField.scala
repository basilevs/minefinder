package minefider.opencv

import minefinder.grid.Grid
import minefinder.field.StatefulField
import org.opencv.core.Mat
import minefinder.field.Mark
import collection.mutable
import minefinder.field.Cell
import minefinder.events.EventSource
import minefinder.events.EventBus
import minefinder.field.Field

/** Handles recognition of items within grid 
 */ 
class RecognizingField(val grid:Grid, val image:(Int, Int, Int, Int)=>Option[Mark])
	extends Field {
    private val field = new StatefulField(grid.xAxis.count, grid.yAxis.count)
    private val eventBus = new EventBus[Cell]()

    override def cells = field.cells    
    def reactions:EventSource[Cell] = eventBus
    def update(cell:Cell) {
        assert(field.cells contains cell)
        val coords = field.unpack(cell)
        val cellRect = (grid.getCell _).tupled(coords)
        val mark = image.tupled(cellRect)
        for (m <- mark) {
            if (field.update(cell, m))
            	eventBus(cell)
        }
    }
}