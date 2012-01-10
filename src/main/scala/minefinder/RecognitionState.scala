package minefinder
import java.awt.image.BufferedImage

class RecognitionState(recognizer:Recognizer) {
	val grid = new GridSearch
	var field = Option.empty[Field]
	var scheduledCells = collection.mutable.Set[(Int, Int)]()
	var results = Array[RecognitionResult]()
	import RecognitionState._
	
	def recognize(img:BufferedImage):Seq[RecognitionResult] = {
		grid.search(img)
		if (isValidGridOption(grid.grid)) {
			val gridI = grid.grid.get
			if (gridI.columns * gridI.rows != results.size) {
				results = new Array[RecognitionResult](gridI.columns * gridI.rows)
				scheduledCells.clear // forces complete refresh
			}
			def refresh(x:Int, y:Int) {
				val pos = y*gridI.columns + x
				results(pos) = recognizer.recognize(gridI.getCellImage(x, y, img))
			}
			if (scheduledCells.size > 0) {
				//Previous recognition results are still valid and we only need to process requested cells
				for ((x,y) <- scheduledCells) {
					refresh(x, y)
				}
				scheduledCells.clear
			} else { // complete refresh
				//If there is no cells scheduled, field might change due to user input. Therefore we should do full refresh if no better option is available. 
				for (
					y <- 0 until gridI.rows;
					x <- 0 until gridI.columns
				) {
					refresh(x,y)
				}
			}
			results
		} else {
			Seq()
		}
	}
	def schedule(x:Int, y:Int) {scheduledCells += ((x,y)) } 
}


object RecognitionState {
	def isValidGrid(g:Grid) = g.rows > 0 && g.columns > 0
	def isValidGridOption(g:Option[Grid]) = g.map(isValidGrid).getOrElse(false)
}