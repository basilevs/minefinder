package recog
import scala.swing.MainFrame
import scala.swing.Label
import scala.swing.SimpleSwingApplication
import java.awt.Image
import javax.imageio.ImageIO
import scala.swing.FlowPanel
import scala.swing.BoxPanel
import scala.swing.Orientation
import java.awt.image.BufferedImage
import org.opencv.features2d.FeatureDetector
import org.opencv.highgui.Highgui
import java.io.ByteArrayOutputStream
import org.opencv.core.Mat
import org.opencv.core.CvType
import org.opencv.core.Range
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.KeyPoint
import java.awt.Graphics2D
import org.opencv.features2d.Features2d
import org.opencv.core.MatOfByte
import java.io.ByteArrayInputStream
import org.opencv.core.Core
import scala.swing.event.ValueChanged
import scala.swing.CheckBox
import scala.swing.Reactions
import scala.swing.event.SelectionChanged
import scala.swing.event.ButtonClicked
import scala.swing.Panel
import scala.swing.ComboBox
import scala.language.implicitConversions 

object MainWindow extends SimpleSwingApplication {
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  def top = new MainWindow()
  implicit def sampleToIcon(s: Image) = {
    new Label() {
      icon = new javax.swing.ImageIcon(s)
    }
  }
  def loadExample(fileName: String) = {
    val imageStream = getClass.getResourceAsStream(fileName)
    ImageIO.read(imageStream)
  }

  implicit def imageToMat(image: BufferedImage): Mat = {
    val bmpOutputStream = new ByteArrayOutputStream()
    ImageIO.write(image, "bmp", bmpOutputStream)
    val encoded = new MatOfByte()
    encoded.fromArray(bmpOutputStream.toByteArray: _*)
    try {
      Highgui.imdecode(encoded, -1)
    } finally {
      encoded.release()
    }
  }

  implicit def matToImage(mat: Mat): BufferedImage = {
    val encoded: MatOfByte = new MatOfByte()
    Highgui.imencode(".bmp", mat, encoded)
    ImageIO.read(new ByteArrayInputStream(encoded.toArray()))
  }

  def detectFeatures(image: Mat) = {
    val detector = FeatureDetector.create(FeatureDetector.GRID_FAST);
    val rv = new MatOfKeyPoint()
    detector.detect(image, rv)
    rv
  }

  def drawFeatures(mat: Mat, features: MatOfKeyPoint): Mat = {
    if (mat.empty())
      throw new IllegalArgumentException()
    val rv: Mat = mat.clone()
    Features2d.drawKeypoints(mat, features, rv)
    rv
  }

  def reduceContrast(mat: Mat, scale: Double): Mat = {
    mat.mul(Mat.ones(mat.size(), mat.`type`()), scale)
  }

  def detectAndDrawFeatures(image: BufferedImage): BufferedImage = {
    val mat: Mat = image
    val features = detectFeatures(mat)
    drawFeatures(reduceContrast(mat, 0.3), features)
  }

}

class MainWindow extends MainFrame {
  import MainWindow._
  val updateReaction: Reactions.Reaction = {
    case e: SelectionChanged => updateImage
    case e: ValueChanged => updateImage
    case e: ButtonClicked => updateImage
  }
  title = "Comparator"
  val imageIcon = new javax.swing.ImageIcon()
  val imageLabel = new Label() { icon = imageIcon }
  val drawOriginalCheck = new CheckBox
  val sourceCombo = new ComboBox(Seq("Example.png", "ExampleGreen.png"))
  sourceCombo.selection.reactions += updateReaction
  val resultsToDraw: collection.mutable.Set[ImageProcessor[_, _]] = collection.mutable.Set()
  drawOriginalCheck.text = "Draw original"
  drawOriginalCheck.reactions += updateReaction

  val canny = new CannyProcessor
  val hough = new HoughProcessor
  val grid = new GridProcessor 

  val composed = new ComposedProcessor(hough, canny) + grid

  val processors = Seq(canny, hough, grid)
  contents = new BoxPanel(Orientation.Vertical) {
    contents += imageLabel
    for (processor <- processors) {
      contents += new ProcPanel(processor)
    }
    contents += drawOriginalCheck
    contents += sourceCombo

  }

  def updateImage {
    val orig: Mat = loadExample(sourceCombo.selection.item)
    var mat: Mat = new Mat(orig, Range.all())
    var image: Mat = if (drawOriginalCheck.selected) { orig.clone() } else Mat.zeros(orig.size(), CvType.CV_8UC3)
    try {
	    composed(mat, (a: ImageProcessor[_, _], b: Any) =>
	      {
	        if (resultsToDraw contains a)
	          a.draw(image, b)
	      }
	    )
    } finally {
	    imageIcon.setImage(image)
	    imageLabel.repaint()
	    MainWindow.this.pack()
    }
  }
  updateImage
  class ProcPanel[T, V](val processor: ImageProcessor[T, V]) extends BoxPanel(Orientation.Horizontal) {
    contents += new Label { text = processor.name + ": " }
    for (parameter <- processor.parameters) {
      val paramComp = new DoubleParameterComponent(parameter)
      paramComp.reactions += updateReaction
      contents += paramComp
    }
    val drawResultCheck = new CheckBox
    drawResultCheck.text = "Draw result: "
    drawResultCheck.reactions += {
      case e: ButtonClicked => {
        val changed = if (drawResultCheck.selected) {
          resultsToDraw.add(processor)
        } else {
          resultsToDraw.remove(processor)
        }
        if (changed)
        	updateReaction(e)
      }
    }
    contents += drawResultCheck
    def isChecked = drawResultCheck.selected
  }
}
