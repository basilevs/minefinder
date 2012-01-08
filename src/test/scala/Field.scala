import javax.imageio.ImageIO
import math.abs

class Field(val name:String, val rows:Int, val columns: Int, val xstep: Float = 18, val ystep:Float = 18) {
	def resUrl = classOf[Field].getResource("minefinder/"+name)
	
	def image = {
		val r = resUrl
		ImageIO.read(r)
	}
	def checkStepX(toCheck:Double) = abs(xstep - toCheck) < 0.1
	def checkStepY(toCheck:Double) = abs(ystep - toCheck) < 0.1
}

object Field {
	val all = Seq(
		new Field("blue_upscale.png", 16, 30, 45.3F, 45.3F),
		new Field("blue_light_right.png", 16, 30),
		new Field("blue_light_left.png", 16, 30),
		new Field("some.png", 16, 30),
		new Field("green.png", 16, 30),
		new Field("blue_medium.png", 16, 16)
	)

}