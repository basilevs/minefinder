package minefinder;

import math.{abs}

import java.awt.Image
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}

class AxisGuess(val start:Float, val step:Float) {
	assert(step>0)
	def distance(x : Int):Double = {
		val mod = abs((x - start) % step)
		math.min(mod.toDouble, (mod - step).toDouble)
	}
	def calcIntensity(intensity:Array[Double]) = {
		var sum = 0.
		var count = 0
		for (xIdx <- 0 until ((intensity.length-start) / step).toInt) {
			val x = start + xIdx*step
			sum += intensity(x.toInt)
			count += 1
		}
		sum/count
	}
	def findAxis(intensity:Array[Double], minIntensity:Double):Axis = {
		var position = start % step
		var found = false
		var newstart = 0.
		while (position < intensity.length) {
			val hit = intensity(position.toInt) > minIntensity 
			if (hit && !found) {
				newstart = position
				assert((newstart - start) % step ==0)
				found = true
			}
			if (!hit && found ) {
				val stop = position - step
				assert((stop - start) % step ==0)
				return new Axis(newstart.toFloat, step, ((stop - newstart) / step).toInt)
			}
			position += step
		}
		return new Axis(newstart.toFloat, step, ((position - step  - newstart) / step).toInt)
	}
	def findBound(hasHit: (Int) => Boolean): Axis = {
		val fuzzy = step/10
		assert(fuzzy > 0)
		var position = start % step
		var newstart = 0.
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
				return new Axis(newstart.toFloat, step, ((stop - newstart) / step).toInt)
			}
			position += step
		}
		assert(false)
		null
	}
}

object AxisGuess {
	def emitHypothesis(minStart:Float, maxStart:Float, minStep:Float, maxStep:Float) =  {
		val startStep = 0.2
		val stepStep = 0.5
		for (
			startIdx <- 0 until ((maxStart-minStart)/startStep).toInt;
			stepIdx  <- 0 until ((maxStep - minStep)/stepStep).toInt
		) yield {
			val start = minStart + startIdx * startStep
			val step = minStep + stepIdx * stepStep
			new AxisGuess(start.toFloat, step.toFloat)
		}
	}
	def makeIntensityArrays(img:BufferedImage, pixelIntensity:(Int)=>Double) = {
		val byX = new Array[Double](img.getWidth)
		val byY = new Array[Double](img.getHeight)
		for (
			y <- 0 until img.getHeight;
			x <- 0 until img.getWidth
		) {
			val i = pixelIntensity(img.getRGB(x, y))
			byX(x) += i / img.getHeight
			byY(y) += i / img.getWidth
		}
		Seq(byX, byY)
//		byX.map(x=>println(x.toString))
	}
	def guessAxisPeriod(intensity:Array[Double]):AxisGuess = {
		val hypotesis:Iterable[AxisGuess] = emitHypothesis(9, intensity.length/10, 9, intensity.length/10)
		hypotesis.map(h => (h, h.calcIntensity(intensity))).maxBy(_._2)._1
	}
}

class Axis(val start:Float, val step:Float, val count:Int) {
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
		val rv = new BufferedImage(width.toInt, height.toInt, BufferedImage.TYPE_INT_RGB)
		val gc = rv.createGraphics
		val cellTop = top + height*y
		val cellLeft = left + width*x 
		gc.drawImage(img, null, -cellLeft.toInt, -cellTop.toInt)
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
	def detectGrid(img:BufferedImage, pixelIntensity:(Int) => Double, minLineIntensity:Double): Grid = {
		val axisIntesities = AxisGuess.makeIntensityArrays(img, pixelIntensity)
		val guesses = axisIntesities.map(AxisGuess.guessAxisPeriod)
		val axises = guesses.zip(axisIntesities).map(pair => pair._1.findAxis(pair._2, minLineIntensity)) 
		new Grid(axises(0), axises(1))
	}
	def detectGrid(img:BufferedImage):Grid = {
		def darkIntensity(rgb:Int):Double = {
			sumRgb((0xFFFFFF-rgb) & 0xFFFFFF)
		}
		detectGrid(img, darkIntensity, 300)
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
	def findPeriod(peaks:Seq[Peak]): Option[AxisGuess] = {
		var bestderivation = 1000.
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