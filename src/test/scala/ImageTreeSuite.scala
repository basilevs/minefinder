package minefinder
import scala.runtime.BoxedUnit
import scala.collection.Seq
import scala.reflect.Manifest
import scala.collection.immutable.Map
import java.lang.reflect.Method
import org.scalatest.FunSuite

import java.awt.Rectangle
import ImageTree._

class ImageTreeSuite extends FunSuite {
	def area(r:Rectangle) = {
		r.getWidth * r.getHeight
	}
	def testSplit(split: Rectangle => Seq[Rectangle]) {
		val source = new Rectangle(2, 6, 52, 75)
		val childs = split(source) 
		assert(childs.map(area).sum === area(source)) 
	}
	def colorToString(color:Int) = {
		import ImageTools._
		"%d, %d, %d".format(red.get(color), green.get(color), blue.get(color))
	}
	def colorToString(color:MeanColor) = {
		val c =color
		val cs = Seq(c.r, c.g, c.b) 
		"%f, %f, %f".format(cs.map(_/c.count):_*)
	}
	test("areaSplit1") {
		testSplit(ImageTree.split1)
	}
	test("meanColor") {
		val img = minefinder.TestField.all(0).image
		val t = new MeanColor.Tree(img)
		val truemean = t.calcMean(img, toRectangle(img))
		assert(colorToString(t.mean) === colorToString(truemean))
		assert(t.mean === truemean)
	}

}