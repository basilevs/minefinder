import org.scalatest.FunSuite

import minefinder._
import java.awt.image.{BufferedImage}
import Recognizer._
import GridSearch._



class RecognizerTest extends FunSuite {
	def validateRecognition(r:Recognizer, m:Mark, img:BufferedImage) {
		r.train(m, img)
		val result = r.recognize(img)
		assert(result.get == m)
	}
	def testRecognizer(r:Recognizer) {
		SampleStorage.instance.foreach(pair => validateRecognition(r, pair._1, pair._2))
	}
	val training = collection.mutable.Buffer[(Mark, BufferedImage)]()
	val testing = collection.mutable.Buffer[(Mark, BufferedImage)]()
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
			val rv = subject.recognize(img)
			if (!rv.isEmpty) {
				if (rv.get == mark) {
					correct+=1
				} else {
					wrong += 1
					println(prefix+": recognized: "+rv.get+", truth: "+ mark)
				}
			}
		}
		for (pair <- training) subject.train(pair._1, pair._2)
		def go = {
			assert(testing.size > 0)
			val bm = new scala.testing.Benchmark() {
				def run {
					for (pair <- testing) recognize(pair._2, pair._1)
				}
			}
			val times = bm.runBenchmark(3)
			prefix+": tests: " +total+ ", recognition rate: " + (correct/total) + ", error rate: " +(wrong/total) +", "+ times
		}
		def print = println(go)
	}
	test("ColorDifference") {
		val subject = new ColorDifference(2)
		testRecognizer(subject)
		println(new RecognizerQuality("ColorDifference", new ColorDifference(30)).go)
	}
	test("Clip") {
		val subject = new Clip() {
			val next = Seq(new ColorDifference(2))
		}
		testRecognizer(subject)
	}
	test("Gray") {
		val subject = new GrayDifference(2)
		testRecognizer(subject)
		println(new RecognizerQuality("GrayDifference", new GrayDifference(10)).go)
	}
	test("ClippedGray") {
		class ClippedGray(th:Int) extends Clip {
			val next = Seq(new GrayDifference(th))
		}
		testRecognizer(new ClippedGray(2))
		println(new RecognizerQuality("ClippedGray", new ClippedGray(2)).go)
	}
	test("ScalingTest") {
		class ScalingTest(th:Int) extends Scaling {
			val height = 9
			val width = 9
			val next = Seq(new ColorDifference(th))
		}
		testRecognizer(new ScalingTest(2))
		println(new RecognizerQuality("ScalingTest", new ScalingTest(30)).go)
	}
	test("BrightnessNormalizer") {
		val subject = new BrightnessNormalizer() {
			val next = Seq(new ColorDifference(2))
		}
		testRecognizer(subject)
	}
	test ("Automatic Recognizer", Active) {
		val subject = new AutomaticRecognizer() 
		try {
			testRecognizer(subject)
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
	ignore("ask user about a few samples") {
		val t = new Training()
		println("Loaded "+ t.persistent.storage.size+" teaching samples")
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
		t.persistent.storage.save
		println("Saved "+ t.persistent.storage.size+" teaching samples")
	}
}
