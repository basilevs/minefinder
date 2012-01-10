package minefinder
import java.awt.image.BufferedImage

class RecognitionState(recognizer:Recognizer) {
	val grid = new GridSearch
	var field = Option.empty[Field]
	var scheduledCells = collection.mutable.Set[(Int, Int)]()
	var results = Array[RecognitionResult]()
	def recognize(img:BufferedImage):Seq[RecognitionResult] = {
		grid.search(img)
		if (grid.grid.isDefined) {
			val gridI = grid.grid.get
			if (gridI.columns * gridI.rows != results.size) {
				reset
			}
			if (results.size > 0) {
				for (
					y <- 0 until gridI.rows;
					x <- 0 until gridI.columns
				) {
					val pos = y*gridI.columns + x
					if (scheduledCells.size > 0) {
						if (scheduledCells.contains((x, y))) {
							results(pos) = recognizer.recognize(gridI.getCellImage(x, y, img))
						}
					} else {
						results(pos) = recognizer.recognize(gridI.getCellImage(x, y, img))
					}
				}
				scheduledCells.clear
				results
			} else {
				null
			}
		} else {
			reset
			null
		}
	}
	def schedule(x:Int, y:Int) {scheduledCells += ((x,y)) } 
	def reset = {
		if (grid.grid.isDefined) {
			results = new Array[RecognitionResult](grid.grid.get.columns * grid.grid.get.rows)
		} else {
			results = Array[RecognitionResult]()
		}
		scheduledCells.clear
	}
}