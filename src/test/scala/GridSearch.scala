import org.scalatest.FunSuite
import java.awt.image.{BufferedImage}
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D, Color, BasicStroke}
import scala.swing._
import javax.swing.ImageIcon
import minefinder.ImageTools._
import minefinder.AxisGuess._
import java.awt.Graphics
import minefinder.AxisGuess
import math.max

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
	def scaleFrom(values:Seq[Double], minCurrentValue:Double, maxCurrentValue:Double, maxTargetValue:Double) = {
		def scaleValue(x:Double) = {
			(x-minCurrentValue)/(maxCurrentValue-minCurrentValue)*maxTargetValue
		}
		values.map(scaleValue)		
	}
	def scaleTo(values:Seq[Double], maxTargetValue:Double): Seq[Double] = {
		val maxCurrentValue = values.max
		val minCurrentValue = values.min
		scaleFrom(values, minCurrentValue, maxCurrentValue, maxTargetValue)
	}
	def scaleAllTo(values:Seq[Seq[Double]], maxTargetValue:Double): Seq[Seq[Double]] = {
		val maxCurrentValue = values.flatten.max
		val minCurrentValue = values.flatten.min
		def doScale(values:Seq[Double]) = scaleFrom(values, minCurrentValue, maxCurrentValue, maxTargetValue)
		values.map(doScale)
	}
	def drawHistogram(values:Seq[Double], gc:Graphics, height:Int) {
		for (i <- 1 until values.size) {
			gc.drawLine(i-1, height - values(i-1).toInt, i, height - values(i).toInt)
		}
	}
	def integrateLinesIntensity(intensity:Array[Double], guess:AxisGuess) = {
		var sum = 0.
		var count = 0
		val maxDist = max(guess.step / 20, 1)
		for (	x <- 0 until intensity.length
		) yield {
			if (guess.distance(x) < maxDist) {
				sum += intensity(x.toInt)
				count+=1
			}
			if (count == 0) 0 else sum/count
		}
	}
	def integrateCellsIntensity(intensity:Array[Double], guess:AxisGuess) = {
		var sum = 0.
		var count = 0
		for (	x <- 0 until intensity.length
		) yield {
			if (guess.distance(x) > guess.step/4) { 
				sum += intensity(x)
				count+=1
			}
			if (count == 0) 0 else sum/count
		}
	}

	ignore("draw intensities") {
		for(field <- fields) {
			val name = field.name
			val img = field.image
			val intensities = makeIntensityArrays(img, classicDarkIntensity)
			val gc = img.createGraphics
			gc.setColor(Color.red)
			gc.setStroke(new BasicStroke(1))
			def draw(smth:Seq[Double]) {
				drawHistogram(smth, gc, img.getHeight)	
			}
			draw(scaleTo(intensities(0), img.getHeight - 2))
			if (false) {
				val guess = guessAxisPeriod(intensities(0)) 
				val guess2 = new AxisGuess(guess.start+guess.step, guess.step*2)
				val graphs = Seq(
					integrateLinesIntensity(intensities(0), guess),
					integrateLinesIntensity(intensities(0), guess2),
					integrateCellsIntensity(intensities(0), guess),
					integrateCellsIntensity(intensities(0), guess2)
				)
				import Color._
				val colors = Seq(green, white, blue, black)
				val scaledGraphs = scaleAllTo(graphs, img.getHeight)
				scaledGraphs.zip(colors).foreach { pair => {
					gc.setColor(pair._2)
					draw(pair._1)
				}}
			}
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
				g.setColor(Color.white)
				g.drawString("X: %f, %f".format(axises(0).start, axises(0).step), 100, 100)
				g.drawString("Y: %f, %f".format(axises(1).start, axises(1).step), 100, 200)
				g.dispose()
//				show("Step "+name, img)
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