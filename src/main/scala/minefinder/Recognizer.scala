package minefinder;
import java.awt.image.{BufferedImage}

import java.awt.Color
import math.{abs, min}
import javax.swing.ImageIcon
import swing._
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label}
import OnceCloseable._
import Option._

case class Sample(mark:Mark, img:BufferedImage) {
	def _1 = mark
	def _2 = img
}

trait Recognizer {
	def recognize(img:BufferedImage):RecognitionResult
	def train(mark:Mark, img:BufferedImage)
	def needTrain:Boolean
	def save
	def name:String
}

class RecognitionResult(
	val query:BufferedImage,
	val result:Option[Mark],
	val recognizer:Recognizer,
	val reason:Option[RecognitionResult] = None,
	val pattern:Option[BufferedImage]= None
) {
	override def equals(that:Any) = that match {
		case thatRes:RecognitionResult => result == thatRes.result
		case _ => false
	}
	def iterator = new Iterator[RecognitionResult] {
		var _next:RecognitionResult = RecognitionResult.this
		def next = {
			val rv = _next;
			_next = rv.reason.getOrElse(null)
			if (_next == RecognitionResult.this)
				_next = null
			rv
		}
		def hasNext = _next != null
	} 
	def isDefined = result.isDefined
	class ViewPanel extends BoxPanel(Orientation.Horizontal) {
		contents += new Label{text = recognizer.name}
		contents += new Label{icon = new ImageIcon(query); text = result.getOrElse("None").toString}
		if (pattern.isDefined)
			contents += new Label{icon = new ImageIcon(pattern.get)}
	}
	private def getViewPanel = new ViewPanel()
	class View extends Dialog {
		contents = new BoxPanel(Orientation.Vertical) {
			contents ++= RecognitionResult.this.iterator.map(_.getViewPanel)
		}
	}
	def getView = new View()
}

object Recognizer {
	implicit def toSample(pair:(Mark, BufferedImage)) = new Sample(pair._1, pair._2)

	import ImageTools._
	import Error._
	abstract class Difference(name:String = null)  extends Recognizer {
		type Pair = Sample
		private val storage = new SampleStorage(name) 
		val exactMatchThreshold:Float
		val probableMatchThreshold:Float
		val patterns = collection.mutable.Buffer[Pair]()
		val list = new ListDialog()	
		def needTrain = patterns.size == 0
		def load {
			if (name != null) {
				patterns.clear
				tryWith(storage.load){patterns ++= _}
			}
		}
		load
		def save {
			if (name != null) {
				storage.save(patterns.iterator)
			}
		}
		def recognizeToPair(img:BufferedImage):Option[(Pair, Float)] = {
			var found = Option.empty[(Pair, Float)]
			for ( pair <- patterns ) {
				val mark = pair._1
				val pattern = pair._2
				val diff = abs(difference(pattern, img))
				if (diff < exactMatchThreshold)
					return Option((pair, diff))
				if (diff < probableMatchThreshold) {
					if (!found.isEmpty && found.get._1.mark != mark) {
//						Thread.dumpStack
//						println("Contradictory recognition: " + found.get._1 +", "+mark)
						list.title = this.getClass.getName + " matches with threshold: " +probableMatchThreshold
//						list.open
						val toShow = possibleMatches(Seq((found.get._1, found.get._2), (pair, diff)), img)
						list.add(toShow)
					}
					if (found.isEmpty || found.get._2 > diff) {
						found = Option((pair, diff))					
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
				val contr = res.get._1._1 != mark
				if (res.get._2 > exactMatchThreshold) {
					new Sample (mark, img) +=: patterns 
				} else if (contr) {
					println("Contradictory training")				
				}
			}
		}
		def recognize(img:BufferedImage) = {
			if (patterns.size==0) {
				println("Warning: difference recognition without patterns")
				new RecognitionResult(img, None, this)
			} else {
				val pair = recognizeToPair(img)
				for (p <- pair) {
					if (p._2 < exactMatchThreshold) {
						patterns -= p._1
						p._1 +=: patterns
					}
					
				}
				new RecognitionResult(img, pair.map(_._1.mark), this, None, pair.map(_._1.img))
			}
		}
		def difference(img1:BufferedImage, img2:BufferedImage):Float
	}
	
	class ColorDifference(maxPixelDiff:Float, name:String = null) extends Difference(name) { 
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) =  {
			if (imageToDimension(img1) == imageToDimension(img2)) {
				val rv = differencePerPixel(img1, img2)
	//			println("Color difference: "+rv)
				rv
			} else 
				probableMatchThreshold + 100
		}
		def name = "ColorDifference("+maxPixelDiff+")"
	}
	
	class GrayDifference(maxPixelDiff:Float, name:String = null) extends Difference(name) {
		def name = "GrayDifference(%f)".format(maxPixelDiff)
		val probableMatchThreshold = maxPixelDiff
		val exactMatchThreshold = 2.F
		def difference(img1:BufferedImage, img2:BufferedImage) = {
			if (imageToDimension(img1) == imageToDimension(img2)) {
				val rv = grayDifferencePerPixel(img1, img2)
	//			println("Gray difference: "+rv)
				rv
			} else {
				probableMatchThreshold + 100
			} 
		}
	}
	trait Cascade  extends Recognizer {
		val next:Iterable[Recognizer]
		def recognize(img:BufferedImage):RecognitionResult = {
			for (n <- next) {
				val rv = n.recognize(img)
				if (rv.isDefined)
					return rv
			}
			new RecognitionResult(img, None, this)
		}
		def needTrain = next.exists(_.needTrain)
		def train(mark:Mark, img:BufferedImage) {
			next.foreach(_.train(mark, img))
		}
		def save {
			next.foreach(_.save)
		} 
	}
	
	trait Transforming extends Cascade {
		override def recognize(img:BufferedImage) = {
			val t = transform(img)
			if (t == null) {
				new RecognitionResult(img, None, this)
			} else {
				var res = super.recognize(t)
				new RecognitionResult(img, res.result, this, Option(res))
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
		def name = "Clip"
		def transform(img:BufferedImage) = {
			clip(img, min(img.getHeight, img.getWidth)/9)
		}
	}
	class StandardDeviation {
		import math._
		private var sumSq = 0.
		private var sum = 0.
		private var n = 0
		def sq = {
			val rv = 1./(n-1)*(sumSq - sum*sum/n)
			assert(!rv.isNaN)
			assert(rv >= 0)
			rv
		}
		def apply =  sqrt(sq)
		def mean = sum/n
		def += (x:Double) {sumSq += x*x; sum += x; n += 1}
	}
	class ColorStandardDeviation {
		private val dev = Vector(new StandardDeviation, new StandardDeviation, new StandardDeviation)
		def += (color:Int) = {
			dev(0) += color & 0xFF0000
			dev(1) += color & 0x00FF00
			dev(2) += color & 0x0000FF
		}
		def apply = math.sqrt(dev.map(_.sq).sum) 
	}
	trait AreaSelector extends Transforming {
		def name = "AreaSelector"
		import AreaSelector._
		def transform(img:BufferedImage) = {
			val dispersions = getDispersions(img)
			val intervals =  dispersions.map(getMaxTwoMinIdx).map(p => (p._2, p._3))
			
			if (intervals(0) == (0, img.getWidth) && intervals(1) == (0, img.getHeight)) {
				null
			} else if (intervals.exists(p=>((p._2 - p._1) < 5) || p._1 < 0 || p._2 < 0)) {
				null
			} else {
				val rv = new BufferedImage(intervals(0)._2 - intervals(0)._1, intervals(1)._2 - intervals(1)._1, BufferedImage.TYPE_INT_RGB)
				val g = rv.createGraphics
				g.drawImage(img, -intervals(0)._1, -intervals(1)._1, Color.black, null)
				g.dispose
				rv
			}	
		}
	} 
	object AreaSelector {
		def getDispersions(img:BufferedImage) = {
			val byX = Array.fill(img.getWidth){new ColorStandardDeviation}
			val byY = Array.fill(img.getHeight){new ColorStandardDeviation}
			for (
				y <- 0 until img.getHeight;
				x <- 0 until img.getWidth
			) {
				val color=img.getRGB(x, y)
				byX(x) += color
				byY(y) += color
			}
			Seq(byX.map(_.apply), byY.map(_.apply))
		}
		def getMaxTwoMinIdx(data:Array[Double]) = {
			var max = -1
			var min1 = -1
			var min2 = -1
			for (i <- 0 until data.size) {
				val x = data(i)
				if (max < 0 || x > data(max)) {
					max = i
					if (min2 > 0 && (min1 < 0 || data(min2) < data(min1))) {
						min1 = min2
					}
					min2 = -1
				}
				if (min2 < 0 || x < data(min2))
					min2 = i
			}
			assert(min1 <= max)
			assert(min2 >= max)
			(max, min1, min2)
		}
	}
	
	trait Scaling extends Transforming {
		val height:Int
		val width:Int
		def name = "Scaling(%d, %d)".format(height, width)
		def transform(img:BufferedImage) = {
//			println("Height: "+img.getHeight+", width: "+ img.getWidth)
			if (abs(img.getHeight - height) < 2 && abs(img.getWidth - width) < 2)
				img 
			else
				toSize(img, width, height)
		}
	}
	
	class AskUser extends Recognizer {
		def name = "AskUser"
		var userQuestions = 0
		def recognize(img:BufferedImage) = {
			val auto = SampleStorage.instance.find(img)
			new RecognitionResult(img, auto.map(_.mark).orElse(ask(img)), this, None)
		}
		def needTrain = false
		def train(mark:Mark, img:BufferedImage) {}
		def save {
			SampleStorage.instance.save
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
			if (!selectedMark.isEmpty)
				SampleStorage.instance += new Sample(selectedMark.get, img) 
			selectedMark
		}
	}
	
	trait BrightnessNormalizer extends Transforming {
		def name = "BrightnessNormalizer"
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
		def name = "AutomaticRecognizer"
		def colorRecognitions = {
			println("colors")
			
		}
		val downScaled = new Scaling {
			val height = 9
			val width = 9
			val next = Seq(new ColorDifference(50, "09-scale-color-50"), new GrayDifference(8, "06-scale-gray-8"))
		}
		val brightnorm = new BrightnessNormalizer() {
			val next = Seq(new GrayDifference(8, "10-clip-norm-gray-8")) 
		}
		val clip = new Clip() {
			val next = Seq(new ColorDifference(50, "08-clip-color-50"), new GrayDifference(8, "07-clip-gray-8"), brightnorm) 
		}
		val area = new AreaSelector {
			val next = Seq(new ColorDifference(50, "11-clip-color-50"), new GrayDifference(8, "12-clip-gray-8"))
		}
		val next = Seq(area, clip, downScaled)
	}
	/** Compares recognition result with user input
	 *  Prints fails on console.
	*/
	trait Verifying extends Cascade {
		val user = new AskUser()
		var total = 0.
		var correct = 0.
		var wrong = 0.
		override def recognize(img:BufferedImage):RecognitionResult = {
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
	/** Uses sample collection and user input to train algorithms.
	 */
	class Training extends Verifying {
		def name = "Training"
		val automatic = new AutomaticRecognizer()
		if (automatic.needTrain)
			SampleStorage.instance.listeners += {sample => automatic.train(sample.mark, sample.img)}
		val next = collection.mutable.Buffer[Recognizer](automatic)
		for (pair <- SampleStorage.instance ) {
			train(pair._1, pair._2)
		}
	}
	
	
}