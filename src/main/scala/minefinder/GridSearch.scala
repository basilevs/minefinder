package minefinder;

import math.{min, abs}

import java.awt.Image
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}

class AxisGuess(val start:Int, val step:Int) {
	def distance(x : Int):Int = {
		val mod = abs((x - start) % step)
		min(mod, mod - step)
	}
	def findBound(hasHit: (Int) => Boolean): Axis = {
		var position = start % step
		var newstart = 0
		var found = false
		while ((position / step) < 1000) {
			val hit = hasHit(position)
			if (hit && !found) {
				newstart = position
				assert((newstart - start) % step ==0)
				found = true
			}
			if (!hit && found ) {
				val stop = position - step
				assert((stop - start) % step ==0)
				assert(stop > newstart)
				return new Axis(newstart, step, (stop - newstart) / step)
			}
			position += step
		}
		assert(false)
		null
	}
}

class Axis(val start:Int, val step:Int, val count:Int) {
	def stop = start+(step*count)
}

class Grid(x:Axis, y:Axis) {
	def left = x.start
	def right = x.stop
	def width = x.step
	def columns = x.count

	def top = y.start
	def bottom = y.stop
	def height = y.step
	def rows = y.count

	def draw(g2d:Graphics2D) {
		for(bin <- 0 until (columns + 1) ) {
//			println(Seq("Line", x.x, 0, x.x, img.getHeight-1))
			val x = bin*width + left
			g2d.draw(new Line2D.Float(x, top, x, bottom))
		}
		for(bin <- 0 until (rows + 1) ) {
			val y = bin*height + top
			g2d.draw(new Line2D.Float(left, y, right, y))
		}
	}
}


class Peak(val x:Int, val width: Int) {
	assert(width > 0)
	def contains(x:Int) = ((this.x - width) < x ) && (x < (this.x + width))
	override def toString = "Peak(" + x + ", " + width+")"
}

object GridSearch {
	//Detects dark lines
	class RgbIntensityFilter(threshold:Int) extends Function1[Int, Boolean] {
		def apply(rgb:Int) = sumRgb(rgb) < threshold
	}
	def sumRgb(rgb:Int):Int = {
		(0xFF & rgb) + 
		((0xFF00 & rgb) >> 8) + 
		((0xFF0000 & rgb) >> 16)
	}
	implicit def predicateToFilter(predicate: (Int) => Boolean) = {
		new TrueIsBlackFilter(predicate)
	}
	class TrueIsBlackFilter(predicate: (Int) => Boolean) extends RGBImageFilter {
		def filterRGB(x:Int, y:Int, rgb:Int) = if (!predicate(rgb)) {
			0xFFFFFFFF
		} else {
			0xFF000000			
		}
		
	}
	class AllColorsLevelFilter(level: Int) extends RGBImageFilter  {
		var pr = 0
		def filterRGB(x:Int, y:Int, rgb:Int) = {
			if ((x % 100) == 0 && (y %100) == 0 && false) {
				println(sumRgb(rgb))
				pr+=1
			}
			if (sumRgb(rgb) > level) {
				0xFFFFFFFF
			} else {
				0xFF000000
			}
		}
	}
	class WhiteMaskFilter(mask:Int) extends RGBImageFilter  {
		def filterRGB(x:Int, y:Int, rgb:Int) = rgb | mask
	}
	class MaskFilter(mask:Int) extends RGBImageFilter  {
		def filterRGB(x:Int, y:Int, rgb:Int) = rgb & mask
	}
	class InvertFilter  extends RGBImageFilter {
		def filterRGB(x:Int, y:Int, rgb:Int) = {
			~(0xFFFFFF&rgb)
		}
	}
	def filter(img:Image, filter: ImageFilter) = {
		imageToBuffered(Toolkit.getDefaultToolkit.createImage(new FilteredImageSource(img.getSource, filter)))
	}
	def calcMeanIntensity(img:BufferedImage) = {
		var sum = 0.
		for (
			y <- 0 until img.getHeight;
			x <- 0 until img.getWidth
		) {
			sum += sumRgb(img.getRGB(x, y))
		}
		sum / img.getHeight / img.getWidth
	}
	def detectPeaks(data:Array[Int], height:Int):Seq[Peak] = {
		var inPeak = false
		var start = 0
		val rv = collection.mutable.Buffer[Peak]()
		for (x <- 0 until data.length) {
			if (data(x)>=height) {
				if (!inPeak) {
					inPeak=true
					start = x
				}
			} else {
				if (inPeak) {
					rv += new Peak((start+x)/2, x - start)
					assert(rv.size > 0)
					inPeak = false
				}
			}
		}
//		println("Peak count: "+ rv.size)
//		println(rv.map(_.toString).foldLeft("")( (head,element) => head+"," +element ))
		rv
	}
	private val bufferedBuilder = new BufferedImageBuilder()
	def imageToBuffered(img:Image) = {
		bufferedBuilder.bufferImage(img)
	}
	def countPixels(img:BufferedImage, rgbFilter:(Int) => Boolean) = {
		val byX = new Array[Int](img.getWidth)
		val byY = new Array[Int](img.getHeight)
		for (
			y <- 0 until img.getHeight;
			x <- 0 until img.getWidth
		) {
			if (rgbFilter(img.getRGB(x, y))) {
				byX(x) += 1
				byY(y) += 1
			}
		}
		(byX, byY)
//		byX.map(x=>println(x.toString))
	}
	//weak - much noise, but make all lines visible
	//strong - no noise
	def detectGrid(img:BufferedImage, weak:(Int) => Boolean, strong:(Int) => Boolean): Grid = {
		val lines = detectLines(img, strong)
		val guess = (findPeriod(lines._1).get, findPeriod(lines._2).get)
		val axises = findBounds(img, guess._1, guess._2, weak)
		new Grid(axises._1, axises._2)
	}
	def detectLines(img:BufferedImage, rgbFilter:(Int) => Boolean) = {
		val (byX, byY) = countPixels(img, rgbFilter)
		(detectPeaks(byX, img.getHeight/2), detectPeaks(byY, img.getWidth/2))		
	}
	class Threshold(data:Seq[Int], threshold:Int) extends Function1[Int, Boolean] {
		override def apply(x:Int) = data(x) > threshold
	}
	def findBounds(img:BufferedImage, x:AxisGuess, y:AxisGuess, rgbFilter:(Int) => Boolean) = {
		val (byX, byY) = countPixels(img, rgbFilter)
		(x.findBound(new Threshold(byX, img.getHeight/4)), y.findBound(new Threshold(byY, img.getWidth/4)))
	}
	def findPeriod(peaks:Seq[Peak]): Option[AxisGuess] = {
		var maxhits = 0
		var bestguess = Option.empty[AxisGuess]
		for (i <- 1 until peaks.size) {
			val prev = peaks(i - 1)
			val curr = peaks(i)
			val guess = new AxisGuess(prev.x, abs(curr.x - prev.x))
			val hits = peaks.count(peak => guess.distance(peak.x) <= peak.width)
			if (hits == peaks.size)
				return Option(guess)
			if (hits > maxhits) {
				bestguess = Option(guess)
				maxhits = hits
			}
		}
		bestguess
	}
}

class GridSearch {
	import GridSearch._
	def search(img:Image) {
		var filtered:Image = null
		
	}
}