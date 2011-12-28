import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.awt.image.{BufferedImage}
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D, Color}
import scala.swing._
import javax.swing.ImageIcon


class GridSearch extends FunSuite {
	import minefinder.GridSearch._
	val names = Seq("some.png", "green.png")
	val images = names.map(name => (name, loadImage(name)))
	def loadImage(name:String) = {
		ImageIO.read(classOf[GridSearch].getResource("minefinder/"+name))
	}
	def show (imageTitle:String, img: Image) {
		val frame = new Frame() {
			this.title = imageTitle
			this.contents = new Label() {
				icon = new ImageIcon(img)
			}
			visible = true
		}
	}
	test("mean intensity") {
		for (pair <- images) {
			println(pair._1+": "+calcMeanIntensity(pair._2))
		}
	}
	def bw(img:BufferedImage) = {
		val intensity = calcMeanIntensity(img)
		filter(img, new AllColorsLevelFilter((intensity/2).toInt))
	}
	test("filter") {
		images.foreach{ case(name, img) => {
				val filtered = bw(img)
				assert(filtered  != null)
//				show("Filtered "+name, filtered )
			}
		}
	}
	test("detect lines") {
		images.foreach{ case(name, img) => {
				val filtered = bw(img)
				assert(filtered  != null)
				val (byX,byY) = detectLines(filtered, x=> (x&0xFFFFFF) > 10)
				val g2d = filtered.createGraphics();
				g2d.setColor(Color.green)
				for(x <- byX) {
					g2d.fill(new Line2D.Float(x.x, x.x, 0, img.getHeight))
				}
				for(y <- byY) {
					g2d.fill(new Line2D.Float(0, img.getWidth , y.x, y.x))
				}
				g2d.dispose();
				show("Lines "+name, filtered )
			}
		}
		
	}
	test("detect grid") {
		
	}
}