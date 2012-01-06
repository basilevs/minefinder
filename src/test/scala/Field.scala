import javax.imageio.ImageIO


class Field(val name:String, val rows:Int, val columns: Int) {
	def resUrl = classOf[Field].getResource("minefinder/"+name)
	
	def image = {
		val r = resUrl
		ImageIO.read(r)
	}
}

object Field {
	val all = Seq(
		new Field("blue_light_right.png", 16, 30),
		new Field("blue_light_left.png", 16, 30),
		new Field("some.png", 16, 30),
		new Field("blue_upscale.png", 16, 30),
		new Field("green.png", 16, 30),
		new Field("blue_medium.png", 16, 16)
	)

}