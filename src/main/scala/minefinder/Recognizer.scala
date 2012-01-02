package minefinder;
import java.awt.image.{BufferedImage}
import math.{abs, min}
import javax.swing.ImageIcon
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label}

case class Sample(mark:Mark, img:BufferedImage) {
	def _1 = mark
	def _2 = img
}

trait Recognizer {
	def recognize(img:BufferedImage): Option[Mark]
	def train(mark:Mark, img:BufferedImage)
}

object Recognizer {
	import ImageTools._
	import Error._
	trait Difference  extends Recognizer {
		type Pair = Sample
		val exactMatchThreshold:Float
		val probableMatchThreshold:Float
		val patterns = collection.mutable.Buffer[Pair]()
		def recognizeToPair(img:BufferedImage):Option[Pair] = {
			var found = Option.empty[Pair]
			var foundDiff = probableMatchThreshold + 1
			for ( pair <- patterns) {
				val mark = pair._1
				val pattern = pair._2
				val diff = abs(difference(pattern, img))
				if (diff < exactMatchThreshold)
					return Option(pair)
				if (diff < probableMatchThreshold) {
					if (found.getOrElse(pair).mark != mark) {
//						Thread.dumpStack
//						println("Contradictory recognition: " + found.get._1 +", "+mark)
						val toShow = possibleMatches(Seq((found.get, foundDiff), (pair, diff)), img)
						showComponent(toShow, false,  this.getClass.getName + " matches with threshold: " +probableMatchThreshold)
					}
					if (foundDiff > diff) {
						found = Option(pair)					
						foundDiff = diff 
					}
				}
			}
			found
		}
		def train(mark:Mark, img:BufferedImage) {
			val res = recognizeToPair(img)
			if (res.isEmpty) {
				patterns += Sample(mark, img)
			} else {
				val contr = res.get._1 != mark
				if (difference(res.get._2, img) > exactMatchThreshold) {
					patterns += Sample (mark, img)
				} else if (contr) {
					println("Contradictory training")				
				}
			}
		}
		def recognize(img:BufferedImage):Option[Mark] = {
			if (patterns.size==0) {
				println("Warning: difference recognition without patterns")
				Option.empty[Mark]
			} else {
				recognizeToPair(img).map(p =>p._1)
			}
		}
		def difference(img1:BufferedImage, img2:BufferedImage):Float
	}
	
	class ColorDifference(maxPixelDiff:Float) extends Difference { 
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			val rv = differencePerPixel(img1, img2)
//			println("Color difference: "+rv)
			rv
		}
	}
	
	class GrayDifference(maxPixelDiff:Float) extends Difference { 
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			val rv = grayDifferencePerPixel(img1, img2)
//			println("Gray difference: "+rv)
			rv
		}
	}
	trait Cascade  extends Recognizer {
		val next:Iterable[Recognizer]
		def recognize(img:BufferedImage): Option[Mark] = {
			next.flatMap(_.recognize(img)).headOption
/*
			val results = next.flatMap(_.recognize(img)).toSet
			if (results.size == 1) {
				results.headOption
			} else {
				Option.empty[Mark]
			}
*/
		}
		def train(mark:Mark, img:BufferedImage) {
			next.foreach(_.train(mark, img))
		}
	}
	
	trait Transforming extends Cascade {
		override def recognize(img:BufferedImage): Option[Mark] = {
			val t = transform(img)
			if (t == null) {
				Option.empty[Mark]
			} else {
				super.recognize(t)
			}
		}
		override def train(mark:Mark, img:BufferedImage) {
			val t = transform(img)
			if (t != null) {
				super.train(mark, t)
			}
		}
		def transform(img:BufferedImage): BufferedImage
	}
	
	trait Clip extends Transforming {
		def transform(img:BufferedImage) = {
			if (img.getHeight > 10 && img.getWidth > 10) {
				clip(img, 2)
			} else {
				null
			}
		}
	}
	
	trait Scaling extends Transforming {
		val height:Int
		val width:Int
		def transform(img:BufferedImage) = {
//			println("Height: "+img.getHeight+", width: "+ img.getWidth)
			if (abs(img.getHeight - height) < 2 && abs(img.getWidth - width) < 2)
				img 
			else
				toSize(img, width, height)
		}
	}
	
	class AskUser extends Cascade {
		var userQuestions = 0
		val next = Seq(new ColorDifference(10), new GrayDifference(5))
		override def recognize(img:BufferedImage): Option[Mark] = {
			val auto = super.recognize(img) 
			if (auto.isEmpty) {
				ask(img)
			} else {
//				println("Recognized with previous user input")
				auto
			}
		}
		var selectedMark = Option.empty[Mark]
		class UserDialog extends Dialog {
			title = "Recognize the symbol"
			modal = true
			val shownIcon = new ImageIcon() //No
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
		val dialog = new UserDialog()
		def ask(img:BufferedImage): Option[Mark] = {
			selectedMark = Option.empty[Mark]
			userQuestions += 1
			dialog.shownIcon.setImage(img)
			dialog.p1.revalidate
			dialog.open
			if (selectedMark.isEmpty) 
				throw new UserFail
			selectedMark
		}
	}
	
	trait BrightnessNormalizer extends Transforming {
		def transform(img:BufferedImage) = {
			val intensity = calcMeanIntensity(img)
			val rv = adjustBrightness(img, 0xFF.toFloat / intensity)
			val tmp = calcMeanIntensity(rv)
			assert(tmp < 1.1 * 0xFF)
			assert(tmp > 0.9 * 0xFF)
			rv
		}
	}
	class AutomaticRecognizer extends Cascade {
		def colorRecognitions = {
			println("colors")
			Seq(new GrayDifference(9), new ColorDifference(60))
		}
		val downScaled = new Scaling {
			val height = 9
			val width = 9
			val next = colorRecognitions
		}
		val clip = new Clip() {
			val next = colorRecognitions
		}
		val brightnorm = new BrightnessNormalizer() {
			val next = colorRecognitions ++ Seq(downScaled, clip) 
		}
		val next = Seq(downScaled, clip)
	}
	/** Compares recognition result with user input
	 *  Prints fails on console.
	*/
	trait Verifying extends Cascade {
		val user = new AskUser()
		var total = 0.
		var correct = 0.
		var wrong = 0.
		override def recognize(img:BufferedImage): Option[Mark] = {
			val auto = super.recognize(img)
			val manual = user.recognize(img)
			total += 1
			if (auto != manual) {
				println("Recognized: "+auto+", manual: "+manual )
				wrong+=1
			} else {
//				println("Automatic recognition success: " + auto.getOrElse("None"))
				correct += 1
			}
			manual
		}
		override def train(mark:Mark, img:BufferedImage) {
			super.train(mark, img)
			user.train(mark, img)
		}
	}
	class PersistentLearner(val storage:SampleStorage) extends Recognizer {
		def recognize(img:BufferedImage) : Option[Mark] = Option.empty[Mark]
		def train(mark:Mark, img:BufferedImage) {
			storage += ((mark, img))
		}
	}
	/** Uses sample collection and user input to train algorithms.
	 */
	class Training extends Verifying {
		val persistent = new PersistentLearner(SampleStorage.instance)
		val automatic = new AutomaticRecognizer()
		val next = collection.mutable.Set[Recognizer](automatic)
		for (pair <- persistent.storage ) {
			train(pair._1, pair._2)
		}
		next+=persistent
		override def recognize(img:BufferedImage) : Option[Mark] = {
			val rv = super.recognize(img)
			rv.foreach(persistent.train(_, img))
			rv
		}
	}
	
	
}