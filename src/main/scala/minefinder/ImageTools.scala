package minefinder;

import java.awt.Image
import java.awt.image.{BufferedImage, ImageFilter, FilteredImageSource, RGBImageFilter}
import java.awt.Toolkit
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}


object ImageTools {
	private val bufferedBuilder = new BufferedImageBuilder()
	def sumRgb(rgb:Int):Int = {
		(0xFF & rgb) + 
		((0xFF00 & rgb) >> 8) + 
		((0xFF0000 & rgb) >> 16)
	}
	def imageToBuffered(img:Image) = {
		bufferedBuilder.bufferImage(img)
	}
	class RgbIntensityFilter(threshold:Int) extends Function1[Int, Boolean] {
		def apply(rgb:Int) = sumRgb(rgb) < threshold
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

}