package minefinder;

import math.{abs, min}
import java.awt.Image
import java.awt.image.{BufferedImage, DirectColorModel, ImageFilter, FilteredImageSource, RGBImageFilter, Raster, DataBufferInt}
import java.awt.{Toolkit, RenderingHints}
import java.awt.geom.{Line2D}
import java.awt.{Graphics2D}
import java.awt.Color
import java.awt.Dimension


object ImageTools {
	private val bufferedBuilder = new BufferedImageBuilder()
	implicit def toDimension(img:Image) = new java.awt.Dimension(img.getWidth(null), img.getHeight(null))
	class Color(shift:Int) {
		val mask:Int = 0xFF << shift
		def get(rgb:Int):Int = (rgb & mask) >> shift
		def put(value:Int):Int = (value << shift) & mask
		def adjust(rgb:Int, coeff:Float):Int = (rgb & ~mask) | put((get(rgb)* coeff).toInt)
	}
	val blue = new Color(0)
	val green = new Color(8)
	val red = new Color(16)
	val colors = Seq(red, green, blue)
	def adjustRgbBrightness(rgb:Int, coeff:Float):Int = {
		var rv = rgb
		for (c <- colors)  {
			rv = c.adjust(rv, coeff)
		}
		return rv
	}
	def adjustBrightness(img:BufferedImage, coeff:Float):BufferedImage = {
		filter(img, new AdjustBrightnessFilter(coeff))
	}
	def calcMeanIntensity(img:BufferedImage):Float = {
		var sum = 0.
		for (
			y <- 0 until img.getHeight;
			x <- 0 until img.getWidth
		) {
			sum += sumRgb(img.getRGB(x, y))
		}
		(sum / img.getHeight / img.getWidth).toFloat
	}
	def differencePerPixel(img1:BufferedImage, img2:BufferedImage):Float = {
		differencePerPixel(img1, img2, (a, b) => sumRgb(a - b))
	}
	def grayDifferencePerPixel(img1:BufferedImage, img2:BufferedImage):Float = {
		differencePerPixel(img1, img2, (a, b) => (sumRgb(a) - sumRgb(b) ) / 3 )
	}
	def checkForRGB(img:BufferedImage) = {
		if (img.getType != BufferedImage.TYPE_INT_ARGB && img.getType != BufferedImage.TYPE_INT_RGB) {
			false
		} else {
			val cm = img.getColorModel.asInstanceOf[DirectColorModel]
			cm.getRedMask() == 0xff0000 && cm.getGreenMask() == 0xff00 && cm.getBlueMask() == 0xff
		}
		
	}
	def differencePerPixelFast(img1:BufferedImage, img2:BufferedImage, diff:(Int, Int) => Float):Option[Float] = {
		val imgs = Seq(img1, img2)
		if (toDimension(img1) != toDimension(img2) || imgs.exists(!checkForRGB(_))) {
			None
		}  else {
			val buffers = imgs.map(_.getData.getDataBuffer.asInstanceOf[DataBufferInt].getData)
			val length = buffers.map(_.length).min
			var sum = 0.D
			for (idx <- 0 until length) {
				sum+=abs(diff(buffers(0)(idx), buffers(1)(idx)))
			}
			Option((sum / length).toFloat)
		}
		
		
	}
	def differencePerPixel(img1:BufferedImage, img2:BufferedImage, diff:(Int, Int) => Float):Float = {
		differencePerPixelFast(img1, img2, diff).getOrElse(differencePerPixelCompatible(img1, img2, diff))
	}
	def differencePerPixelCompatible(img1:BufferedImage, img2:BufferedImage, diff:(Int, Int) => Float):Float = {
		val height = min(img1.getHeight, img2.getHeight)
		val width = min(img1.getWidth, img2.getWidth)
		var sum = 0.D
		for (
			y <- 0 until height;
			x <- 0 until width
		) {
			sum+=abs(diff(img1.getRGB(x, y), img2.getRGB(x, y)))
		}
		(sum / height / width).toFloat
	}
	def toSize(img:Image, width:Int, height:Int) = {
		val rv = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		val gc = rv.createGraphics
//		gc.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED)
		gc.drawImage(img, 0, 0, width, height, null)
		gc.dispose
		rv
	}
	def clip(img:BufferedImage, margin:Int) = {
		if (margin <= 0) {
			img
		} else {
			val width = img.getWidth - (2 * margin)
			val height = img.getHeight - (2 * margin)
			val rv = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
			val gc = rv.createGraphics
			gc.drawImage(img, -margin, -margin, Color.black, null)
			gc.dispose
			assert(rv.getHeight == img.getHeight - 2 * margin)
			rv
		}
	}
	def compare(img1:BufferedImage, img2:BufferedImage):Int = {
		val h = img1.getHeight - img2.getHeight
		if (h != 0)
			return h
		val w = img1.getWidth - img2.getWidth
		if (w != 0)
			return w
		for (
			y <- 0 until img1.getHeight;
			x <- 0 until img1.getWidth
		) {
			val p = img1.getRGB(x, y) - img2.getRGB(x, y)
			if (p != 0)
				return p
		}
		return 0
	}
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
	implicit def predicateToFilter(predicate: (Int) => Boolean):ImageFilter = {
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
	
	class AdjustBrightnessFilter(coeff:Float) extends RGBImageFilter  {
		def filterRGB(x:Int, y:Int, rgb:Int) = adjustRgbBrightness(rgb, coeff)
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
