import org.scalatest.FunSuite
import java.awt.image.{BufferedImage}
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D, Color, BasicStroke}
import scala.swing._
import javax.swing.ImageIcon
import minefinder.ImageTools._
import minefinder.AxisGuess._
import java.awt.Graphics
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
	def scaleTo(values:Seq[Double], maxTargetValue:Double) = {
		val maxCurrentValue = values.max
		val minCurrentValue = values.min
		def scaleValue(x:Double) = {
			(x-minCurrentValue)/(maxCurrentValue-minCurrentValue)*maxTargetValue
		}
		values.map(scaleValue)
	}
	def drawHistogram(values:Seq[Double], gc:Graphics, height:Int) {
		for (i <- 1 until values.size) {
			gc.drawLine(i-1, height - values(i-1).toInt, i, height - values(i).toInt)
		}
	}
	test("draw intensities") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val intensities = makeIntensityArrays(img, classicDarkIntensity)
			val gc = img.createGraphics
			gc.setColor(Color.red)
			gc.setStroke(new BasicStroke(1))
			drawHistogram(scaleTo(intensities(0), img.getHeight - 2), gc, img.getHeight)
			gc.dispose()
			show("Intensity "+name, img)		
		}
	}
	test("detect period") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val intensities = makeIntensityArrays(img, classicDarkIntensity)
			val axises = intensities.map(guessAxisPeriod(_))
			if (true) {
				val g = img.createGraphics
				g.setColor(Color.green)
				g.setStroke(new BasicStroke(1))
				axises(0).drawX(g)
				axises(1).drawY(g)
				g.dispose()
				show("Step "+name, img)
			}
			assert(field.checkStepX(axises(0).step))
			assert(field.checkStepY(axises(1).step))
		}
	}
	test("detect grid") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val intensity = calcMeanIntensity(img)
//			show("Weak: "+name, filter(img, weak))
			val grid = detectGrid(img)
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