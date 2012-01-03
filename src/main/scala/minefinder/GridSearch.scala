package minefinder;

import math.{min, abs}

import java.awt.Image
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}

class AxisGuess(val start:Int, val step:Int) {
	assert(step>0)
	def distance(x : Int):Int = {
		val mod = abs((x - start) % step)
		min(mod, mod - step)
	}
	def findBound(hasHit: (Int) => Boolean): Axis = {
		val fuzzy = step/10
		assert(fuzzy > 0)
		var position = start % step
		var newstart = 0
		var found = false
		while ((position / step) < 1000) {
			val from = (position - fuzzy).toInt
			val to = (position + fuzzy).toInt
			val hit =  from.to(to).exists(hasHit(_))
			if (hit && !found) {
				newstart = position
				assert((newstart - start) % step ==0)
				found = true
			}
			if (!hit && found ) {
				val stop = position - step
				assert((stop - start) % step ==0)
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
	def getCellImage(x:Int, y:Int, img:BufferedImage) = {
		assert(x>=0)
		assert(y>=0)
		val rv = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		val gc = rv.createGraphics
		val cellTop = top + height*y
		val cellLeft = left + width*x 
		gc.drawImage(img, null, -cellLeft, -cellTop)
		gc.dispose()
		rv
	}
	def getMiddle(x:Int, y:Int) = {
		val cellTop =(0.5*height + top + height*y).toInt
		val cellLeft = (0.5 * width + left + width*x ).toInt
		(cellLeft, cellTop)
	}
}


class Peak(val x:Int, val width: Int) {
	assert(width > 0)
	def contains(x:Int) = ((this.x - width) < x ) && (x < (this.x + width))
	override def toString = "Peak(" + x + ", " + width+")"
}

object GridSearch {
	import ImageTools._
	//Detects dark lines
	def detectPeaks(data:Array[Int], height:Int):Seq[Peak] = {
		var inPeak = false
		var start = 0
		var highest = 0
		val rv = collection.mutable.Buffer[Peak]()
		for (x <- 0 until data.length) {
			if (data(x)>=height) {
				if (!inPeak) {
					inPeak=true
					start = x
				}
				if (data(highest) < data(x))
					highest = x
			} else {
				if (inPeak) {
					rv += new Peak(highest, x - start)
					assert(rv.size > 0)
					inPeak = false
					highest = 0
				}
			}
		}
//		println("Peak count: "+ rv.size)
//		println(rv.map(_.toString).foldLeft("")( (head,element) => head+"," +element ))
		rv
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
	def detectGrid(img:BufferedImage):Grid = {
			val intensity = calcMeanIntensity(img)
			val strong = new RgbIntensityFilter((intensity*0.5).toInt)
//			show("Strong: "+name, filter(img, strong))
			val weak = new RgbIntensityFilter((intensity*0.9).toInt)
			detectGrid(img, weak, strong)
	}
	def detectLines(img:BufferedImage, rgbFilter:(Int) => Boolean) = {
		val (byX, byY) = countPixels(img, rgbFilter)
		(detectPeaks(byX, img.getHeight/2), detectPeaks(byY, img.getWidth/2))		
	}
	class Threshold(data:Seq[Int], threshold:Int) extends Function1[Int, Boolean] {
		override def apply(x:Int) = {
			if (x < 0) false
			else if (x > data.size) false
			else data(x) > threshold
		}
	}
	def scanGuess(guess:AxisGuess, hasHit: (Int) => Boolean):Axis = {
//		println("Scan")
		(
			for (
				start <- (guess.start - 1) to (guess.start + 1);
				period <- guess.step to (guess.step + 1) 
			) yield {
				val t = new AxisGuess(start, period)
				val rv = t.findBound(hasHit)
//				println("Start: %d, step %d, count %d".format(rv.start, rv.step, rv.count))
				rv
			}
		).maxBy(_.count)
	}
	def findBounds(img:BufferedImage, x:AxisGuess, y:AxisGuess, rgbFilter:(Int) => Boolean) = {
		val (byX, byY) = countPixels(img, rgbFilter)
		(scanGuess(x, new Threshold(byX, img.getHeight/2)), scanGuess(y, new Threshold(byY, img.getWidth/2)))
	}
	def findPeriod(peaks:Seq[Peak]): Option[AxisGuess] = {
		var bestderivation = 1000
		var bestguess = Option.empty[AxisGuess]
		for (i <- 1 until peaks.size) {
			val prev = peaks(i - 1)
			val curr = peaks(i)
			val guess = new AxisGuess(prev.x, abs(curr.x - prev.x))
			val derivation = peaks.map(peak => guess.distance(peak.x)/peak.width).sum
			if (derivation == 0)
				return Option(guess)
			if (derivation < bestderivation) {
				bestguess = Option(guess)
				bestderivation = derivation
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