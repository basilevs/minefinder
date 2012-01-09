package minefinder;

import math.{abs, max}
import java.awt.{Image, Dimension}
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}
import ImageTools._
import java.awt.Graphics

class AxisGuess(val start:Double, val step:Double) {
	assert(step>0)
	def distance(x : Int):Double = {
		val mod = abs((x - start) % step)
		math.min(mod, abs((mod - step)))
	}
	def calcLinesIntensity(intensity:Array[Double]) = {
		var sum = 0.
		var count = 0
		val maxDist = max(step / 20, 1)
		for (	x <- 0 until intensity.length;
				if distance(x) < maxDist 
		) {
			sum += intensity(x.toInt)
			count += 1
		}
		sum/count
	}
	def calcCellsIntensity(intensity:Array[Double]) = {
		var sum = 0.
		var count = 0
		for (	x <- 0 until intensity.length;
				if distance(x) > step/4 
		) {
			sum += intensity(x)
			count += 1
		}
		sum/count
	}
	def calcSignalNoiseRate(intensity:Array[Double]) = {
		var signalSum = 0.
		var signalCount = 0
		var noiseSum = 0.
		var noiseCount = 0
		val maxDist = max(step / 20, 1)
		for (	x <- 0 until intensity.length) {
			val d = distance(x) 
			val i = intensity(x)
			if (d < maxDist) {
				signalSum += i
				signalCount += 1
			} else if (d > maxDist*2) {
				noiseSum += i
				noiseCount += 1
			}
		}
		signalSum/signalCount/noiseSum*noiseCount
	}
	def findAxis(intensity:Array[Double], minIntensity:Double):Axis = {
		var position = start % step
		var found = false
		var newstart = 0.
		while (position < intensity.length) {
			val idx = position.toInt
			val a = intensity(idx)
			val b = intensity(idx-1)
			val c = intensity(idx+1)
			val hit = intensity.slice(idx-1, idx+2).exists(_ > minIntensity) 
			if (hit && !found) {
				newstart = position
				found = true
			}
			if (!hit && found ) {
				val stop = position - step
				return new Axis(newstart.toFloat, step, ((stop - newstart) / step).toInt)
			}
			position += step
		}
		return new Axis(newstart.toFloat, step, ((position - step  - newstart) / step).toInt)
	}
	def drawX(g:Graphics) {
		val length = 100
		for (xIdx <- 0 until 10) {
			val x = (start%step + step*xIdx).toInt 
			g.drawLine(x, 0, x, length)
		}
	}
	def drawY(g:Graphics) {
		val length = 100
		for (xIdx <- 0 until 10) {
			val x = (start%step + step*xIdx).toInt 
			g.drawLine(0, x, length, x)
		}
	}
}

object AxisGuess {
	def emitHypothesis(minStep:Float, maxStep:Float) =  {
		val stepStep = 0.05
		val maxStepIdx = (maxStep - minStep)/stepStep
		for (
			stepIdx  <- 0 until maxStepIdx.toInt;
			step = minStep + stepStep.toDouble * stepIdx;
			start <- 0.until(step.toInt, max(1, step/36).toInt)
		) yield {
			new AxisGuess(start.toFloat, step.toFloat)
		}
	}
	def classicDarkIntensity(rgb:Int) = {
		sumRgb(0xFFFFFF) - sumRgb(rgb)
	}
	def invertedWhiteIntensity(rgb:Int):Double = {
		1./(sumRgb(rgb)+0.0001)
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
		val hypotesis:Iterable[AxisGuess] = emitHypothesis(10, intensity.length/10)
		val selected = hypotesis.map(h => (h, h.calcSignalNoiseRate(intensity))).maxBy(_._2)
/*		val test = new AxisGuess(selected._1.start, selected._1.step*2)
		val testrate = test.calcSignalNoiseRate(intensity)
*/		selected._1
	}
}

class Axis(start:Double, step:Double, val count:Int) extends AxisGuess(start, step) {
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
			g2d.draw(new Line2D.Float(x.toFloat, top.toFloat, x.toFloat, bottom.toFloat))
		}
		for(bin <- 0 until (rows + 1) ) {
			val y = bin*height + top
			g2d.draw(new Line2D.Float(left.toFloat, y.toFloat, right.toFloat, y.toFloat))
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


object GridSearch {
	import AxisGuess._
	def detectGrid(img:BufferedImage, pixelIntensityDetect:(Int) => Double): Grid = {
		val guessIntesities = makeIntensityArrays(img, pixelIntensityDetect)
		val guesses = guessIntesities.map(AxisGuess.guessAxisPeriod)
		val axises = guesses.zip(guessIntesities).map(pair => pair._1.findAxis(pair._2, pair._2.max/2)) 
		new Grid(axises(0), axises(1))
	}
	def detectGrid(img:BufferedImage):Grid = {
		detectGrid(img, classicDarkIntensity)
	}
}

/** Caches found grid*/
class GridSearch {
	import GridSearch._
	import ImageTools.imageToDimension
	var dimension = Option.empty[Dimension]
	var grid = Option.empty[Grid]
	def search(img:BufferedImage) = {
		val nd = imageToDimension(img)
		if (Option(nd) != dimension) {
			grid = Option(detectGrid(img))
		}
		grid.get
	}
}