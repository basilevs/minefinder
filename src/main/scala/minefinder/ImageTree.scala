package minefinder
import java.awt.image.BufferedImage
import java.awt.Image
import java.awt.Rectangle
import math.max
import ImageTools.{toDimension, sumRgb}
import math.abs

// Splits image into smaller chunks
// Each chunk is associated with T object which allows quick comparison
abstract class ImageTree[+T](val img:BufferedImage, area:Rectangle) {
	import ImageTree._
	def this(img:BufferedImage) = this(img, ImageTree.toRectangle(img))
	def branch(img:BufferedImage, area:Rectangle): ImageTree[T]
	val childAreas = split1(area) 
	val childs:Seq[ImageTree[T]] = if (childAreas.size > 1) {
		childAreas.map(area => branch(img, area))	
	} else {
		Seq(this)
	}
	def mean:T
}

class MeanColor(val r:Double, val g:Double, val b:Double, val count:Int = 1) {
	assert(count > 0)
	def - (that:MeanColor) = new MeanColor(r - that.r, g - that.g, b - that.b,  math.abs(count - that.count))
	def + (that:MeanColor) = new MeanColor(r + that.r, g + that.g, b + that.b,  count + that.count)
	def intensity = (r+g+b)/count
}

object MeanColor {
	class Tree(img:BufferedImage, area:Rectangle) extends ImageTree[MeanColor](img, area) {
		def this(img:BufferedImage) = this(img, ImageTree.toRectangle(img))
		def calcMean(seq:Seq[MeanColor]) = seq.reduce(_ + _)
		override def branch(img:BufferedImage, area:Rectangle): Tree = new Tree(img, area)			
		override def mean:MeanColor = if (childs.size <= 1) {
			calcMean(img, area)
		} else {
			calcMean(childs.map(_.mean))
		}
		def calcMean(img:BufferedImage, area:Rectangle) = {
			import ImageTools._
			var seq = ImageTree.toSeq(img, area)
			var r=0.
			var g=0.
			var b=0.
			var c = 0
			for(p <- seq) {
				r += red.get(p)
				g += green.get(p)
				b += blue.get(p)
				c+=1
			}
			new MeanColor(r, g, b, c)
		}
	}
	def difference(a:ImageTree[MeanColor], b:ImageTree[MeanColor], colorDifference: (MeanColor, MeanColor) => Double, limit:Double):Double = {
		if (toDimension(a.img) != toDimension(b.img)) {
			limit
		} else {
			assert(a.childs.size == b.childs.size)
			if (a.childs.size == 1) {
				colorDifference(a.mean, b.mean)
			} else {
				{
					for (pair <- a.childs.zip(b.childs)) yield {
						val d = abs(difference(pair._1, pair._2, colorDifference, limit))
						if (d > limit)
							return d
						d
					}
				}.sum/a.childs.size
			}
		}
	}
	def colorDifference(a:Tree, b:Tree, limit:Double):Double = {
		difference(a, b, (x, y) => (x - y).intensity, limit)
	}
	def grayDifference(a:Tree, b:Tree, limit:Double):Double = {
		difference(a, b, (x, y) => (x.intensity - y.intensity)/3, limit)
	}

}


object ImageTree {
	private implicit def double2int(d:Double) = d.toInt
	def toRectangle(img:Image) = new Rectangle(0, 0, img.getWidth(null), img.getHeight(null))
	def split1(area:Rectangle):Seq[Rectangle] = {
		if (max(area.getHeight, area.getWidth) < 5) {
			Seq(area)
		} else if (area.getHeight > area.getWidth) {
			val middle:Int = area.getHeight/2
			Seq(
				new Rectangle(area.getX, area.getY, area.getWidth, middle),
				new Rectangle(area.getX, middle, area.getWidth, area.getHeight - middle)
			)
		} else {
			val middle:Int = area.getWidth/2
			Seq(
				new Rectangle(area.getX, area.getY, middle, area.getHeight),
				new Rectangle(middle, area.getY, area.getWidth - middle, area.getHeight)
			)
		}
	}
	def toSeq(img:BufferedImage, area:Rectangle):Seq[Int] = {
		for (
			y <- area.getY.toInt until area.getMaxY.toInt;
			x <- area.getX.toInt until area.getMaxX.toInt
		) yield img.getRGB(x, y)
	}
	def calcMeanC(colors:Seq[Int]) = {
		var red = 0.
		var green = 0.
		var blue = 0.
		for (c <- colors) {
			red += ImageTools.red.get(c)
			green += ImageTools.green.get(c)
			blue += ImageTools.blue.get(c)
		}
		val s = colors.size
		ImageTools.red.put((red/s).toInt) | ImageTools.green.put((green/s).toInt) | ImageTools.blue.put((blue/s).toInt)  
	}
}