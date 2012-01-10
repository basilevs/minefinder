package minefinder;
import Recognizer._
import java.awt.image.{BufferedImage}

object Controller extends App {
	var doWork = true
	var windowsArePresent = false
	val productionRecognizer = new Cascade {
		def name = "ProductionRecognizer"
		val user = new AskUser()
		val next = Seq(new AutomaticRecognizer, user)
		if (needTrain) {
			for (sample <- SampleStorage.instance) {
				train(sample.mark, sample.img)
			}
		}
		var needSave = false
		SampleStorage.instance.listeners += (sample => {
			println("Got notification "+sample.mark)
			
			train(sample.mark, sample.img)
		})
		override def save {
			if (needSave) {
				super.save
				needSave = false
			}
		}
		
	}
	println("Trained on "+SampleStorage.instance.size+" teaching samples")
	val windows = collection.mutable.Map[Window, RecognitionState]()
	def fieldWindowHook(window:Window) {
		try {
			def newState = new RecognitionState(productionRecognizer)
			val state = windows.getOrElseUpdate(window, newState)
			val img = window.captureImage
			if (img == null) {
				state.reset
				return	
			}
			val marks = state.recognize(img)
			if (marks == null) return
			val grid = state.grid.grid.get
			val f = new Field(grid.columns, marks)
			
			val cells = Field.getCellsWithMineFlag(f)
			println("Clicking: "+ cells)
			if (cells.size> 0 && window.isCompletelyVisible) {
				val s = InputState.get
				try {
					window.bringForeground
					for (c <- cells) {
						state.schedule(c._1.x, c._1.y)
						val (x, y) = grid.getMiddle(c._1.x, c._1.y)
						if (c._2) {
							window.rclick(x, y)
							Thread.sleep(10)
						} else {
							window.lclick(x, y)
							Thread.sleep(10)
						}
						if (!window.isCompletelyVisible) {
							println("Window is unaccessible after clicking "+c)
							val v = new FieldView(f)
							v.visible = true
							state.reset
							throw new Window.MouseClickException()
						}
					}
				} finally {
					s.restore
				}
				if (window.isCompletelyVisible) {
					windowsArePresent = true
					productionRecognizer.save
				}
			}
		} catch { //Exceptions are not propagated from within JNA hook
			case mc:Window.MouseClickException => {}
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
	while (doWork) {
		windowsArePresent = false
		Window.EnumWindows(windowHook)
		if (!windowsArePresent) {
			Thread.sleep(1000)
		}
	}
	println("Job complete")
	System.exit(0)
}