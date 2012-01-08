package minefinder;

import math.{abs, max}
import java.awt.Image
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
		val maxDist = max(step / 14, 1)
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
		val rv = calcLinesIntensity(intensity)/calcCellsIntensity(intensity)
		rv
	}
	def findAxis(intensity:Array[Double], minIntensity:Double):Axis = {
		var position = start % step
		var found = false
		var newstart = 0.
		while (position < intensity.length) {
			val hit = intensity(position.toInt) > minIntensity 
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
		val length = 200
		for (xIdx <- 0 until 10) {
			val x = (start%step + step*xIdx).toInt 
			g.drawLine(x, 0, x, length)
		}
	}
	def drawY(g:Graphics) {
		val length = 200
		for (xIdx <- 0 until 10) {
			val x = (start%step + step*xIdx).toInt 
			g.drawLine(0, x, length, x)
		}
	}
}

object AxisGuess {
	def emitHypothesis(minStart:Float, maxStart:Float, minStep:Float, maxStep:Float) =  {
		val startStep = maxStep/36
		val stepStep = max(startStep/maxStep, 0.01)
		val maxStartIdx = (maxStart-minStart)/startStep
		val maxStepIdx = (maxStep - minStep)/stepStep
		for (
			startIdx <- 0 until maxStartIdx.toInt;
			stepIdx  <- 0 until maxStepIdx.toInt
		) yield {
			val start = minStart + startStep.toDouble * startIdx 
			val step = minStep + stepStep.toDouble * stepIdx  
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
		val hypotesis:Iterable[AxisGuess] = emitHypothesis(10, intensity.length/10, 10, intensity.length/10)
		val selected = hypotesis.map(h => (h, h.calcSignalNoiseRate(intensity))).maxBy(_._2)
/*		val test = new AxisGuess(selected._1.start, selected._1.step*2)
		val testrate = test.calcSignalNoiseRate(intensity)
*/		selected._1
	}
}

class Axis(val start:Double, val step:Double, val count:Int) {
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
	def detectGrid(img:BufferedImage, pixelIntensityDetect:(Int) => Double, lineTestIntensity:(Int) => Double, minLineIntensity:Double): Grid = {
		val guessIntesities = makeIntensityArrays(img, pixelIntensityDetect)
		val guesses = guessIntesities.map(AxisGuess.guessAxisPeriod)
		val axisIntesities = makeIntensityArrays(img, lineTestIntensity)
		val axises = guesses.zip(axisIntesities).map(pair => pair._1.findAxis(pair._2, minLineIntensity)) 
		new Grid(axises(0), axises(1))
	}
	def detectGrid(img:BufferedImage):Grid = {
		val mean = calcMeanIntensity(img)
		detectGrid(img, classicDarkIntensity, classicDarkIntensity, mean/1.8)
	}
}

class GridSearch {
	import GridSearch._
	def search(img:Image) {
		var filtered:Image = null
		
	}
}