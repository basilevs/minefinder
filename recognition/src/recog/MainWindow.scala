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
import collection.mutable
import concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import concurrent.Future
import scala.swing.BorderPanel
import scala.swing.GridPanel
import scala.swing.GridBagPanel
import scala.swing.GridBagPanel

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
    case e: SelectionChanged => schedule
    case e: ValueChanged => schedule
    case e: ButtonClicked => updateImage
  }
  title = "Comparator"
  val imageIcon = new javax.swing.ImageIcon()
  val imageLabel = new Label() { icon = imageIcon }
  val drawOriginalCheck = new CheckBox
  val sourceCombo = new ComboBox(Seq("Example.png", "ExampleGreen.png", "ExampleSmall.png"))
  sourceCombo.selection.reactions += updateReaction
  val resultsToDraw: collection.mutable.Set[ImageProcessor[_, _]] = collection.mutable.Set()
  drawOriginalCheck.text = "Draw original"
  drawOriginalCheck.selected = true
  drawOriginalCheck.reactions += updateReaction

  val processors = collection.mutable.Buffer[ImageProcessor[_, _]]()
  val cannyGrid = new CannyProcessor
  val hough = new HoughProcessor
  val grid = new GridProcessor
  
  val composedGrid = new ComposedProcessor(hook(hough), hook(cannyGrid)) + hook(grid)
  val digitCanny = new CannyProcessor
  val cell = new CellProcessor[Mat, composedGrid.Result](hook(digitCanny), composedGrid) {
    override def getGrid(gridData: composedGrid.Result) = {
      gridData._2
    }
  }
  val crop = new CropProcessor(cell)
  processors += crop
  val rootProcessor = crop

  var lastResults: rootProcessor.Result = null
  var orig: Mat = loadExample(sourceCombo.selection.item)

  contents = new BorderPanel() {
    val options = new BoxPanel(Orientation.Vertical) {
      contents += new GridBagPanel() {
        var i = 0
        for (processor <- processors) {
          val drawResultCheck = new CheckBox
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
          add(drawResultCheck, (0, i))
          add(new Label { text = processor.name + ": " }, (1, i))
          add(new ProcPanel(processor), (2, i))
          i+=1
        }
      }
      contents += drawOriginalCheck
      contents += sourceCombo
    }
    import BorderPanel.Position._
    layout(imageLabel) = Center
    layout(options) = South
  }
  updateImage

  def hook[T, R](processor: ImageProcessor[T, R]): ImageProcessor[T, R] = {
    processors += processor
    new ForwardingProcessor(processor) {
      override def draw(target: Mat, result: R) {
        var isChecked = resultsToDraw contains processor
        if (isChecked)
          processor.draw(target, result)
      }
    }
  }

  def schedule {
    Future(process)
  }

  def process {
    try {
      val image: Mat = loadExample(sourceCombo.selection.item)
      lastResults = null
      orig = image
      updateImage(image, null)
      val result = process(image)
      lastResults = result 
      updateImage(image, result)
    } catch {
      case e:Throwable => {
        e.printStackTrace()
      }
    }
  }

  def process(input: Mat): rootProcessor.Result = {
    var mat: Mat = input.clone()
    rootProcessor(mat)
  }

  def updateImage {
    updateImage(orig, lastResults)
  }

  def updateImage(orig: Mat, result: rootProcessor.Result) {
    var image: Mat = if (drawOriginalCheck.selected) { orig.clone() } else Mat.zeros(orig.size(), CvType.CV_8UC3)
    if (result != null)
      rootProcessor.draw(image, result)
    imageIcon.setImage(image)
    imageLabel.repaint()
  }

  class ProcPanel[T, V](val processor: ImageProcessor[T, V]) extends BoxPanel(Orientation.Horizontal) {
    for (parameter <- processor.parameters) {
      val paramComp = new DoubleParameterComponent(parameter)
      paramComp.reactions += updateReaction
      contents += paramComp
    }
  }
  schedule
}
