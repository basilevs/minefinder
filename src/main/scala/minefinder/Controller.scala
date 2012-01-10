package minefinder;
import Recognizer._
import java.awt.image.{BufferedImage}

object Controller extends App {
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
	def processGameFieldWindow(window:Window) {
		try {
			def newState = new RecognitionState(productionRecognizer)
			val state = windows.getOrElseUpdate(window, newState)
			val img = window.captureImage
			if (img == null) {
				windows.remove(window)
				return	
			}
			val marks = state.recognize(img) //Potentially very long process
			if (marks == null) return
			val grid = state.grid.grid.get
			val f = new Field(grid.columns, marks)
			
			val cells = Field.getCellsWithMineFlag(f) // potentially long (not as long as recognition)
			println("Clicking: "+ cells)
			if (cells.size> 0 && window.isCompletelyVisible && window.isForeground) {
				val s = InputState.get
				try {
					window.bringForeground
					def checkWindow {
						if (!window.isCompletelyVisible) {
							throw new Window.MouseClickException()
						}
					}
					for (c <- cells) {
						state.schedule(c._1.x, c._1.y)
						val (x, y) = grid.getMiddle(c._1.x, c._1.y)
						if (c._2) {
							window.rclick(x, y) // throws MouseClickException
							Thread.sleep(10)
						} else {
							window.lclick(x, y) // throws MouseClickException
							Thread.sleep(10)
						}
						checkWindow
					}
					Thread.sleep(100)
					checkWindow
					productionRecognizer.save // Note, that this doesn't happen if checkWindowThrows 
				}catch { 
					case mc:Window.MouseClickException => {
							val v = new FieldView(f)
							v.visible = true
					}
				} finally {
					s.restore
				}
			}
		} catch { //Exceptions are not propagated from within JNA hook
			case a:Exception => {
				println(a)
			}
		}
	}
	
	def getFirstChild(w:Window) = {
		var rv = Option.empty[Window]
		def childHook(c:Window) = {
			rv = Option(c)
			false
		}
		w.EnumChilds(childHook)
		rv
	}
	while (true) {
		val mainW = Window.foregroundWindow
		if(mainW.text =="Minesweeper" || mainW.text == "Сапер") {
			getFirstChild(mainW).foreach(processGameFieldWindow)
		} else {
			Thread.sleep(1000)
		}
	}
	println("Job complete")
	System.exit(0)
}