package minefinder;
import javax.imageio.ImageIO
import math.abs

class TestField(val name:String, val rows:Int, val columns: Int, val xstep: Float = 18, val ystep:Float = 18) {
	def resUrl = classOf[TestField].getResource(name)
	
	def image = {
		val r = resUrl
		ImageIO.read(r)
	}
	def checkStepX(toCheck:Double) = abs(xstep - toCheck) < 0.1
	def checkStepY(toCheck:Double) = abs(ystep - toCheck) < 0.1
}

object TestField {
	val all = Seq(
//		new TestField("0-closed-problem.png", 16, 16, 18.1F, 18F),
		new TestField("blue_medium.png", 16, 16, 18.1F, 18F),
		new TestField("green.png", 16, 30),
		new TestField("blue_light_right.png", 16, 30),
		new TestField("blue_light_left.png", 16, 30, 18, 18.1F),
		new TestField("some.png", 16, 30, 18, 18.1F),
		new TestField("blue_upscale.png", 16, 30, 45.3F, 45.3F)
	)
}