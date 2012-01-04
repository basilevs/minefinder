package minefinder;
import Recognizer._
import java.awt.image.{BufferedImage}

object Controller extends App {
	var doWork = true
	val productionRecognizer = new Cascade {
		val user = new AskUser()
		val next = Seq(new AutomaticRecognizer, user)
		for (sample <- SampleStorage.instance) {
			train(sample.mark, sample.img)
		}
		SampleStorage.instance.listeners += (sample => {
			println("Got notification "+sample.mark)
			train(sample.mark, sample.img)
		})
		
	}
	def fieldWindowHook(window:Window) {
		try {
			val img = window.captureImage
			if (img == null) return
			val grid = GridSearch.detectGrid(img)
			val marks = 
			for (
				y <- 0 until grid.rows;
				x <- 0 until grid.columns
			) yield {
				val cImg = grid.getCellImage(x, y, img)
				productionRecognizer.recognize(cImg)
			}
			SampleStorage.instance.save
			val f = new Field(grid.columns, marks.toSeq)
			val cells = Field.getCellsWithMineFlag(f)
			for (c <- cells) {
				val (x, y) = grid.getMiddle(c._1.x, c._1.y)
				if (c._2) {
					window.rclick(x, y)
				} else {
					window.lclick(x, y)
				}
			}
		} catch { //Exceptions are not propagated from within JNA hook
			case a:Exception => {
				doWork = false
				println(a)
			}
		}
	}
	def windowHook(window:Window) = {
		if (window.text =="Minesweeper" || window.text == "Сапер") {
			def child(w:Window) = {
				fieldWindowHook(w)
				false
			}
			window.EnumChilds(child)
		}
		doWork
	}
	while (doWork)
		Window.EnumWindows(windowHook)
	println("Job complete")
}