package minefinder.field

class StatefulField(val width:Int, val height:Int) extends Field {
	private class StatefulCell(val x:Int, val y:Int) extends Cell {
	    validatePos(x, y)
	    var mark: Option[Mark] = None
	    var neighboors: Set[Cell] = null
	    def update(m:Mark) = {
	        val rv = mark
	        mark = Some(m)
	        rv
	    }
	}
	
	private val matrix:Array[Array[StatefulCell]] = Array.ofDim(width, height)
	override val cells: Set[Cell] = { 
        for (x <- 0 until width;
             y <- 0 until height
             ) yield {
            val rv = new StatefulCell(x, y)
            matrix(x)(y) = rv
            rv
        }
    }.toSet
    
    for (cell <- cells)
        calcNeighboors(cell.asInstanceOf[StatefulCell])
	
	private def validatePos (x: Int, y:Int) {
	    assert(x>=0)
	    assert(y>=0)
	    assert(x<width)
	    assert(y<height)
	}

    

    private def calcNeighboors(cell: StatefulCell) {
        val candidates = {for(
        		x <- (cell.x-1) until (cell.x+1);
        		y <- (cell.y-1) until (cell.y+1);
        		if (!(x == cell.x && y == cell.y))
       ) yield getCellOption(x, y)}
       cell.neighboors = candidates.flatten.toSet 
    }
    
    
    
    private def getCellOption(x:Int, y:Int): Option[StatefulCell] = {
	    if (x>=0 && y>=0 && x<width && y<height) 
	        Some(matrix(x)(y))
	    else 
	        None
    }
    
    private def get(x: Int, y: Int): Cell = {
	    doGet(x: Int, y: Int)
    }
    
    private def doGet(x: Int, y: Int): StatefulCell = {
	    validatePos(x, y)
	    matrix(x)(y)
    }
    
    /** @return cell coordinates tuple (x,y)*/
    def unpack(cell:Cell) = {
        val sc = cell.asInstanceOf[StatefulCell]
        (sc.x, sc.y)
    }
    
    /** Sets the new value to the cell
     *  @param x,y position to update
     *  @param m new value
     *  @return true if changed
     */
    def update(cell:Cell, m:Mark) = {
        val sc = cell.asInstanceOf[StatefulCell]
        val old = sc.update(m)
        val unchanged = old.map( _ == m).getOrElse(false)
        !unchanged
    }
}