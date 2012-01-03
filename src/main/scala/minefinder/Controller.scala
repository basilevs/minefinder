package minefinder;
import Recognizer._
import java.awt.image.{BufferedImage}

object Controller extends App {
	var doWork = true 
	val productionRecognizer = new Cascade {
		val user = new AskUser() {
			override def recognize(img:BufferedImage) = {
				val rv = super.recognize(img)
				if (!rv.isEmpty) {
					SampleStorage.instance += new Sample(rv.get, img)
					println("Added sample:"+rv)
				}
				rv
			}
		}
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
			val grid = GridSearch.detectGrid(img)
			val marks = 
			for (
				y <- 0 until grid.rows;
				x <- 0 until grid.columns
			) yield {
				val cImg = grid.getCellImage(x, y, img)
				productionRecognizer.recognize(cImg)
			}
			val f = new Field(grid.columns, marks.toSeq)
			val cells = Field.getEmptyClosedCells(f)
			for (c <- cells) {
				val (x, y) = grid.getMiddle(c.x, c.y)
				window.click(x, y)
			}
			SampleStorage.instance.save
		} catch {
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
	
}