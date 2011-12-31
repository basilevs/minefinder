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
	class TrainError extends RuntimeException
	class ContradictoryTraining extends TrainError
	class UserFail extends TrainError
	import ImageTools._
	trait Difference  extends Recognizer {
		val exactMatchThreshold:Float
		val probableMatchThreshold:Float
		val patterns = collection.mutable.Buffer[(Mark, BufferedImage)]()
		def train(mark:Mark, img:BufferedImage) {
			for (pattern <- patterns) {
				if (abs(difference(img, pattern._2))<exactMatchThreshold) {
					if (pattern._1 != mark)
						throw new ContradictoryTraining()
					println("Rejected similar image during difference training. Trained: "+ patterns.size)
//					Thread.dumpStack()
					return
				}
			}
			patterns += ((mark, img))
		}
		def recognize(img:BufferedImage):Option[Mark] = {
			if (patterns.size==0) {
				println("Warning: difference recognition without patterns")
				Option.empty[Mark]
			} else {
				val matches = collection.mutable.Set[Mark]()
				for ( (mark, pattern) <- patterns) {
					val diff = abs(difference(pattern, img))
					if (diff < exactMatchThreshold)
						return Option(mark)
					if (diff < probableMatchThreshold) {
						matches += mark
						if (matches.size > 1)
							return Option.empty[Mark]
					}
				}
				matches.headOption
			}
		}
		def difference(img1:BufferedImage, img2:BufferedImage):Float
	}
	
	class ColorDifference(maxPixelDiff:Float) extends Difference { 
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			differencePerPixel(img1, img2)
		}
	}
	
	class GrayDifference(maxPixelDiff:Float) extends Difference { 
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			grayDifferencePerPixel(img1, img2)
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
		val next = Seq(new ColorDifference(10))
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
	
	class AutomaticRecognizer extends Cascade {
		val simple = new Scaling {
			val height = 18
			val width = 18
			val next = Seq(new ColorDifference(30))
		}
		val downScaled = new Scaling {
			val height = 9
			val width = 9
			val next = Seq(new ColorDifference(30))
		}
		val clip = new Clip() {
			val next = Seq(new ColorDifference(30))
		}
		val grayClip = new Clip() {
			val next = Seq(new GrayDifference(30))
		}
		val next = Seq(simple, downScaled, clip, grayClip) 
	}
	/** Compares recognition result with user input
	 *  Prints fails on console.
	*/
	trait Verifying extends Cascade {
		val user = new AskUser()
		var total = 0.
		var correct = 0.
		override def recognize(img:BufferedImage): Option[Mark] = {
			val auto = super.recognize(img)
			val manual = user.recognize(img)
			total += 1
			if (auto != manual) {
				println("Recognized:"+auto+", manual:"+manual )
			} else {
				println("Automatic recognition success:" + auto)
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