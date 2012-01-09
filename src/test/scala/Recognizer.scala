import org.scalatest.FunSuite
import minefinder._
import java.awt.image.{BufferedImage}
import Recognizer._
import GridSearch._
import java.awt.Color
import java.awt.BasicStroke



class RecognizerTest extends FunSuite {
	def validateRecognition(r:Recognizer, m:Mark, img:BufferedImage) {
		r.train(m, img)
		val result = r.recognize(img)
		if (result.result.isDefined)
		if (result.result.get != m) {
			Thread.dumpStack
			assert(false)
		}
	}
	val training = collection.mutable.Buffer[Sample]()
	val testing = collection.mutable.Buffer[Sample]()
	var count = 0
	if(SampleStorage.instance.size >0) {
		for (sample <- SampleStorage.instance) {
			if ((count % 2) == 0) {
				training += sample
			} else {
				testing += sample
			}
			count += 1
		}
		assert(count>0)
		assert(training.size>0)
		assert(testing.size>0)
	}
	class RecognizerQuality(val prefix:String, val subject:Recognizer) {
		var total = 0
		var correct = 0
		var wrong = 0
		assert(training.size>0)
		def recognize(img:BufferedImage, mark:Mark) {
			total +=1
			assert(total>0)
			val rv = subject.recognize(img)
			if (rv.isDefined) {
				if (rv.result.get == mark) {
					correct+=1
					assert(correct>0)
				} else {
					wrong += 1
					assert(wrong>0)
					println(prefix+": recognized: "+rv.result.get+", truth: "+ mark)
				}
			}
		}
		if (subject.needTrain) {
			for (pair <- training) {
				validateRecognition(subject, pair._1, pair._2)
			}
		}
		val format = "%20s: rate: %.3f, errrrate: %.3f, times: %s"
		def go = {
			assert(testing.size > 0)
			val bm = new scala.testing.Benchmark() {
				def run {
					for (pair <- testing) recognize(pair._2, pair._1)
				}
			}
			val times = bm.runBenchmark(1)
			format.format(prefix, correct.toFloat/total, wrong.toFloat/total, times.toString)
		}
		def print = println(go)
	}
	test("colorDifference function") {
		val img = Field.all(0).image
		assert(ImageTools.differencePerPixel(img, img)==0)
		
	}
	test("ColorDifference") {
		new RecognizerQuality("ColorDifference", new ColorDifference(75)).print
	}
	test("Gray") {
		new RecognizerQuality("Gray", new GrayDifference(10)).print
	}
	test("ClippedColor") {
		val subject = new Clip() {
			val next = Seq(new ColorDifference(50))
		}
		new RecognizerQuality("ClippedColor", subject).print
		val img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB)
		val g = img.createGraphics
		g.setColor(Color.green)
		g.setStroke(new BasicStroke(2))
		g.drawRect(0,0,19, 19)
		g.setColor(Color.blue)
		g.drawRect(5,5,14, 14)
		g.dispose
		subject.recognize(img).getView.open
	}
	test("AreaSelectorColor") {
		val subject = new AreaSelector {
			val next = Seq(new ColorDifference(50))
		}
		new RecognizerQuality("AreaSelectorColor", subject).print
		for (sample <- SampleStorage.instance.filter(_.mark == Number(1)).toSeq.splitAt(10)._1) {
			subject.recognize(sample.img).getView.open
		}
	}
	test("AreaSelectorGray") {
		val subject = new AreaSelector {
			val next = Seq(new GrayDifference(8))
		}
		new RecognizerQuality("AreaSelectorGray", subject).print
	}
	test("AreaSelectorGrayShowResult") {
		val subject = new AreaSelector {
			val next = Seq(new GrayDifference(8))
		}
	}
	test("ClippedGray") {
		class ClippedGray extends Clip {
			val next = Seq(new GrayDifference(8))
		}
		new RecognizerQuality("ClippedGray", new ClippedGray).print
	}
	test("ScalingTest") {
		class ScalingTest(th:Int) extends Scaling {
			val height = 9
			val width = 9
			val next = Seq(new ColorDifference(th))
		}
		new RecognizerQuality("ScalingTest", new ScalingTest(60)).print
	}
	test("BrightnessNormalizer") {
		val subject = new BrightnessNormalizer() {
			val next = Seq(new GrayDifference(8))
		}
		new RecognizerQuality("BrightnessNormalizer", subject).print
	}
	test("ClippedBrightnessNormalizer") {
		val subject = new Clip() {
			val next = Seq(
				new BrightnessNormalizer() {
					val next= Seq(new GrayDifference(8))
				}
			)
		}
		new RecognizerQuality("ClippedBrightnessNormalizer", subject).print
	}
	test ("Automatic Recognizer", Active) {
		val subject = new AutomaticRecognizer() 
		try {
			new RecognizerQuality("AutomaticRecognizer", subject).print
		} catch {
			case e:ContradictoryRecognition => {
				Error.handle(e)
				throw e
			}
		}
	}
	def imageToCells(img:BufferedImage):Iterable[BufferedImage] = {
		val grid  = detectGrid(img)
		(
			for (
				y <- 0 until grid.rows;
				x <- 0 until grid.columns
			) yield grid.getCellImage(x, y, img)
		)
	}
	test("ask user about a few samples") {
		val t = new Training()
		println("Loaded "+ SampleStorage.instance.size+" teaching samples")
		val cells = Field.all.flatMap(f => imageToCells(f.image))
		val iter = cells.iterator
		var count = 0
		while (t.user.userQuestions < 15 && iter.hasNext) {
			val img = iter.next
			t.recognize(img)
			count += 1
			if (count % 100 == 0) 
				println("Count: "+count)
		}
		println("Recognition rate: " + (t.correct/t.total)+ ", wrong rate: " + t.wrong/t.total)
		SampleStorage.instance.save
		println("Saved "+ SampleStorage.instance.size+" teaching samples")
	}
}
