package minefinder;

import java.awt.Image
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit

class Grid(val left:Int, val top:Int, val width:Int, val height:Int, val columns:Int, val rows:Int) {
	def bottom = top + (height * rows)
	def right = left + (width * columns)
}

class Peak(val x:Int, val width: Int) {
	assert(width > 0)
	def contains(x:Int) = ((this.x - width) < x ) && (x < (this.x + width))
}

object GridSearch {
	def sumRgb(rgb:Int):Int = {
		(0xFF & rgb) + 
		((0xFF00 & rgb) >> 8) + 
		((0xFF0000 & rgb) >> 16)
	}
	class AllColorsLevelFilter(level: Int) extends RGBImageFilter  {
		var pr = 0
		def filterRGB(x:Int, y:Int, rgb:Int) = {
			if ((x % 100) == 0 && (y %100) == 0) {
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
		Toolkit.getDefaultToolkit.createImage(new FilteredImageSource(img.getSource, filter))
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
	def detectPeaks(data:Array[Int], height:Int) = {
		var inPeak = false
		var start = 0
		val rv = collection.mutable.Seq[Peak]()
		for (x <- 0 until data.length) {
			if (data(x)>=height) {
				if (!inPeak) {
					inPeak=true
					start = x
				}
			} else {
				if (inPeak) {
					rv :+ new Peak((start+x)/2, x - start)
				}
			}
		}
		rv
	}
	private val bufferedBuilder = new BufferedImageBuilder()
	implicit def imageToBuffered(img:Image) = {
		bufferedBuilder.bufferImage(img)
	}
	def detectLines(img:BufferedImage, rgbFilter:(Int) => Boolean) = {
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
		(detectPeaks(byX, img.getHeight/2), detectPeaks(byY, img.getWidth/2))
	}
}

class GridSearch {
	import GridSearch._
	def search(img:Image) {
		var filtered:Image = null
		
	}
}