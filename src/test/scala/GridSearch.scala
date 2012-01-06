import org.scalatest.FunSuite
import java.awt.image.{BufferedImage}
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D, Color, BasicStroke}
import scala.swing._
import javax.swing.ImageIcon

import minefinder.ImageTools._

class GridSearch extends FunSuite {
	import minefinder.GridSearch._
	val fields = Field.all
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
		for (field <- fields) {
			println(field.name+": "+calcMeanIntensity(field.image))
		}
	}
	def bw(img:BufferedImage) = {
		val intensity = calcMeanIntensity(img)
		filter(img, new AllColorsLevelFilter((intensity/2).toInt))
	}
	def bw(img:Image, predicate:(Int) => Boolean) = {
		filter(img, predicate)
	}
	test("filter") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val filtered = bw(img)
			assert(filtered  != null)
//			show("Filtered "+name, filtered )
		}
	}
	test("detect lines") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val filtered:BufferedImage = bw(img)
			assert(filtered  != null)
			val (byX,byY) = detectLines(filtered, x=> (x&0xFFFFFF) < 10)
			val g2d = filtered.createGraphics();
			g2d.setColor(Color.green)
			g2d.setStroke(new BasicStroke(2))
//				println("Width: "+img.getWidth+", Height: "+img.getHeight)
			for(x <- byX) {
//					println(Seq("Line", x.x, 0, x.x, img.getHeight-1))
				g2d.draw(new Line2D.Float(x.x, 0, x.x, img.getHeight-1))
			}
			for(y <- byY) {
				g2d.draw(new Line2D.Float(0, y.x, img.getWidth-1 , y.x))
			}
			g2d.dispose();
//				show("Lines "+name, filtered )
		}

	}
	test("detect grid") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val intensity = calcMeanIntensity(img)
			val strong = new RgbIntensityFilter((intensity*0.5).toInt)
			show("Strong: "+name, filter(img, strong))
			val weak = new RgbIntensityFilter((intensity*0.9).toInt)
//			show("Weak: "+name, filter(img, weak))
			val grid = detectGrid(img, weak, strong)
			val g2d = img.createGraphics();
			g2d.setColor(Color.green)
			g2d.setStroke(new BasicStroke(2))
			grid.draw(g2d)
			g2d.dispose()
			show("Grid "+name, img)
			assert(grid.rows == field.rows)
			assert(grid.columns == field.columns)
		}
	}
	test("select cell") {
		for(field <- fields) {
			val img = field.image
			val grid  = detectGrid(img)
			val cellImage = grid.getCellImage(4, 6, img)
			assert(cellImage!=null)
//			show("Cell 4, 6 of "+field.name+":", cellImage)
		}
	}
}