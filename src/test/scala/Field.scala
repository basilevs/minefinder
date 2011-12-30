import javax.imageio.ImageIO


class Field(val name:String, val rows:Int, val columns: Int) {
	def image = ImageIO.read(classOf[Field].getResource("minefinder/"+name))
}

object Field {
	val all = Seq(
		new Field("some.png", 16, 30),
		new Field("green.png", 16, 30),
		new Field("blue_medium.png", 16, 16)
	)

}