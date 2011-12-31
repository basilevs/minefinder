import org.scalatest.FunSuite

import minefinder._
import java.awt.image.{BufferedImage}
import Recognizer._
import GridSearch._

class RecognizerTest extends FunSuite {
	def loadSamples:Set[(Mark, BufferedImage)] = {
		val ss = SampleStorage.instance
		val builder = Set.newBuilder[(Mark, BufferedImage)]
		builder.result
	}
	def validateRecognition(r:Recognizer, m:Mark, img:BufferedImage) {
		r.train(m, img)
		val result = r.recognize(img)
		assert(result.get == m)
	}
	def testRecognizer(r:Recognizer) {
		loadSamples.foreach(pair => validateRecognition(r, pair._1, pair._2))
	}
	test("ColorDifference") {
		val subject = new ColorDifference(2)
		testRecognizer(subject)
	}
	test("Clip") {
		val subject = new Clip() {
			val next = Seq(new ColorDifference(30))
		}
		testRecognizer(subject)
	}
	test("Gray") {
		val subject = new GrayDifference(30)
		testRecognizer(subject)
	}
	test("ClippedGray") {
		val subject = new Clip() {
			val next = Seq(new GrayDifference(30))
		}
		testRecognizer(subject)
	}
	test("ScalingTest") {
		val subject = new Scaling {
			val height = 9
			val width = 9
			val next = Seq(new ColorDifference(30))
		}
		testRecognizer(subject)
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
		println("Loaded "+ t.persistent.storage.size+" teaching samples")
		val cells = Field.all.flatMap(f => imageToCells(f.image))
		val iter = cells.iterator
		while (t.user.userQuestions < 15 && iter.hasNext) {
			val img = iter.next
			t.recognize(img)
		}
		println("Recognition rate: " + t.correct/t.total)
		t.persistent.storage.save
		println("Saved "+ t.persistent.storage.size+" teaching samples")
	}
}
