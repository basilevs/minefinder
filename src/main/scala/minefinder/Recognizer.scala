package minefinder;
import java.awt.image.{BufferedImage}
import math.{abs, min}
import javax.swing.ImageIcon
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label}

trait Recognizer {
	def recognize(img:BufferedImage): Option[Mark]
	def train(mark:Mark, img:BufferedImage)
}

object Recognizer {
	import ImageTools._
	trait Difference  extends Recognizer {
		val patterns = collection.mutable.Buffer[(Mark, BufferedImage)]()
		def train(mark:Mark, img:BufferedImage) {
			patterns += ((mark, img))
		}
		def recognize(img:BufferedImage) = {
			if (patterns.size==0) {
				Option.empty[Mark]
			} else {
				val (mark, diff) = patterns.iterator.map( p => (p._1, abs(difference(p._2, img))) ).minBy(_._2)
				if (diff < 1.) {
					Option(mark)
				} else {
					Option.empty[Mark]
				}
			}
		}
		def difference(img1:BufferedImage, img2:BufferedImage):Float
	}
	
	class ColorDifference(maxPixelDiff:Float) extends Difference { 
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			val height = min(img1.getHeight, img2.getHeight)
			val width = min(img1.getWidth, img2.getWidth)
			(
				for (
					y <- 0 until height;
					x <- 0 until width
				) yield {
					abs(sumRgb(img1.getRGB(x, y) - img2.getRGB(x, y)))
				}
			).sum / height / width * maxPixelDiff
		}
	}
	
	trait Cascade  extends Recognizer {
		val next:Iterable[Recognizer]
		def recognize(img:BufferedImage): Option[Mark] = {
			val results = next.flatMap(_.recognize(img)).toSet
			if (results.size == 1) {
				results.headOption
			} else {
				Option.empty[Mark]
			}
		}
		def train(mark:Mark, img:BufferedImage) {
			next.foreach(_.train(mark, img))
		}
	}
	
	trait Transforming extends Cascade {
		override def recognize(img:BufferedImage): Option[Mark] = {
			val t = transform(img)
			super.recognize(t)
		}
		override def train(mark:Mark, img:BufferedImage) {
			val t = transform(img)
			super.train(mark, t)
		}
		def transform(img:BufferedImage): BufferedImage
	}
	
	trait RemoveBoundaryNoise extends Transforming {
		def isNoise:Function[Int, Boolean]
		def transform(img:BufferedImage) = {
			img
		}
	}
	
	trait Scaling extends Transforming {
		val height:Int
		val width:Int
		def transform(img:BufferedImage) = {
			assert(img.getHeight <= height+1)
			assert(img.getHeight >= height-1)
			assert(img.getWidth <= width+1)
			assert(img.getWidth >= width-1)			
			img
		}
	}
	
	class AskUser extends Recognizer {
		def recognize(img:BufferedImage): Option[Mark] = {
			ask(img)
		}
		def train(mark:Mark, img:BufferedImage) {}
		val shownIcon = new ImageIcon()
		var selectedMark = Option.empty[Mark]
		val frame = new Dialog() {
			title = "Recognize the symbol"
			modal = true
			val p1 = new BoxPanel(Orientation.Vertical) {
				def optionToButton(text:String, opt:Option[Mark]) = {
					new Button(text) {
						action = new Action(text) {
							def apply {
								selectedMark = opt
								close()
							}
						}
					}
				}
				def markToButton(mark:Mark) = {
					optionToButton(mark.toString, Option(mark))
				}
				contents += new Label{icon=shownIcon}
				contents ++= Mark.all.map(markToButton)
				contents += optionToButton("Don't know", Option.empty[Mark])
			}
			contents = p1
		}

		def ask(img:BufferedImage): Option[Mark] = {
			selectedMark = Option.empty[Mark]
			shownIcon.setImage(img)
			frame.open
			selectedMark
		}
	}
	
	class AutomaticRecognizer extends Cascade {
		val simple = new Scaling {
			val height = 7
			val width = 7
			val next = Seq(new ColorDifference(30))
		}
		val next = Seq(simple) 
	}
	trait Verifying extends Cascade {
		val user = new AskUser()
		override def recognize(img:BufferedImage): Option[Mark] = {
			val auto = super.recognize(img)
			val manual = user.recognize(img)
			if (auto == manual) {
				auto
			} else {
				Option.empty[Mark]
			}
		}
	}
	class PersistentLearner(name:String) extends Recognizer {
		val storage = new SampleStorage(name)
		storage.load
		def recognize(img:BufferedImage) : Option[Mark] = Option.empty[Mark]
		def train(mark:Mark, img:BufferedImage) {
			storage += ((mark, img))
		}
	}
	class Training extends Verifying {
		val persistent = new PersistentLearner("samples")
		val automatic = new AutomaticRecognizer()
		val next = Seq(automatic, persistent)
		override def recognize(img:BufferedImage) : Option[Mark] = {
			val rv = super.recognize(img)
			rv.foreach(train(_, img))
			rv
		}
	}
	
	
}