package minefinder
import java.nio.file.Files.createTempFile
import javax.imageio.ImageIO
import org.scalatest.FunSuite
import java.awt.image.{BufferedImage, ImageFilter, RGBImageFilter, FilteredImageSource}
import java.awt.Image
import scala.swing._
import javax.swing.ImageIcon
import java.awt.Toolkit
import minefinder.Window
import minefinder.FullUser32
import minefinder.OnceCloseable
import minefinder.Region


class WindowSuite extends FunSuite {
	test("EnumWindows") {
		var works = false
		Window.EnumWindows(
			x => {
				val title = x.text
				if (title.length > 5) {
					works = true
					println(title)
					true
				} else {
					true
				}
			}
		)
		assert(works)
	}
	
	test("LookForMineSweeper") {
		assert(!Window.GetMineSweeper.isEmpty)
	}
	
	test("recursiveEnum") {
		def recursiveEnum(w:Window, depth:Int):Boolean = {
//			println("_"*depth+w.text)
			if (depth < 3) {
				w.EnumChilds(recursiveEnum(_, depth+1))
			}
			true
		}
		Window.EnumWindows(recursiveEnum(_, 0))
	}
	
	test("enumthread") {
		println("Thread windows:")
		def recursiveEnum(w:Window, depth:Int):Boolean = {
//			println("_"*depth+"Title: "+w.text+", Class: "+w.className)
			w.EnumChilds(recursiveEnum(_, depth+1))
			true
		}
		val mine = Window.GetMineSweeper.get
		mine.EnumThreadWindows(recursiveEnum(_, 0))
	}
	def capture = {
		val mine = Window.GetMineSweeper.get
		var img:Option[BufferedImage] = None
		def capture(w:Window) = {
			img = w.captureImage
			false
		}
		mine.EnumChilds(capture)
		img
	}
	def show (imageTitle:String, img: Image) {
		val frame = new Frame() {
			this.title = imageTitle
			this.contents = new Label() {
				icon = new ImageIcon(img)
			}
			visible = true
		}
	}
	import OnceCloseable._
	test("regionEquality") {
		def create1 = Region.createRectRegion(1, 1, 10, 10)
		def create2 = Region.createRectRegion(1, 1, 30, 10)
		tryWith(create1, create1) { (reg1, reg2) => {
			assert(reg1.equals(reg2))
		}}
		tryWith(create1, create2) { (reg1, reg2) => {
			assert(!reg1.equals(reg2))
		}}
	}
	test("regionBox") {
		tryWith(Region.createRectRegion(1, 1, 10, 10)) { reg => {
			val box = reg.getBox
			assert(box.left == 1)
			assert(box.top == 1)
			assert(box.right == 10)
			assert(box.bottom == 10)
		}}
	}
	test("capture") {
		var img:Option[BufferedImage] = capture
		assert(img.isDefined)
		if (true) {
			val path  = createTempFile("mf", ".png")
			println("Created "+path)
			ImageIO.write(img.get, "PNG", path.toFile)
		}
	}
	test("focus") {
		val mine = Window.GetMineSweeper
		if (mine.isDefined) {
			
		}
	}
	test("click") {
		val mine = Window.GetMineSweeper.get
		val m = minefinder.InputState.get
		var exception = Option.empty[Exception]
		def clickChild(w:Window) = {
			try {
				w.bringForeground
				w.rclick(100, 100)
				Thread.sleep(10)
			} catch {
				case e:Exception => exception = Option(e)
			}
			false
		}
		mine.EnumChilds(clickChild)
		m.restore
		exception.map(throw _)
	}
	ignore ("foreground") {
		Window.GetMineSweeper.map(_.bringForeground)
	}
}