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
import concurrent.Future

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
    val sourceCombo = new ComboBox(Seq("Example.png", "ExampleGreen.png"))
    sourceCombo.selection.reactions += updateReaction
    val resultsToDraw: collection.mutable.Set[ImageProcessor[_, _]] = collection.mutable.Set()
    drawOriginalCheck.text = "Draw original"
    drawOriginalCheck.selected = true
    drawOriginalCheck.reactions += updateReaction

    val canny = new CannyProcessor
    val hough = new HoughProcessor
    val grid = new GridProcessor
    val processors = Seq(canny, hough, grid)

    var lastResults:Seq[(ImageProcessor[_, _], Any)] = Seq()
    var orig:Mat = loadExample(sourceCombo.selection.item)

    val composed = new ComposedProcessor(hook(hough), hook(canny)) + hook(grid)

    contents = new BoxPanel(Orientation.Vertical) {
        contents += imageLabel
        for (processor <- processors) {
            contents += new ProcPanel(processor)
        }
        contents += drawOriginalCheck
        contents += sourceCombo
    }
    updateImage
    
    var mutableHook: Option[(ImageProcessor[_, _], Any) => Unit] = None


    def hook[T, R](processor: ImageProcessor[T, R]) = {
	    def processResult(processor: ImageProcessor[_, _], result: Any) {
	        mutableHook.foreach(_(processor, result))
	    }
        new HookedProcessor(processor, processResult)
    }

    def drawResult(processor: ImageProcessor[_, _], result: Any, image: Mat) {
        if (resultsToDraw contains processor)
            processor.draw(image, result)
    }

    def schedule {
        Future(process)
    }
    
    def process {
        val image:Mat = loadExample(sourceCombo.selection.item)
        orig = image
        lastResults = process(image)
        updateImage(image, lastResults)
    }

    
    def process(input: Mat): Seq[(ImageProcessor[_, _], Any)] = {
        val results: mutable.Buffer[(ImageProcessor[_, _], Any)] = mutable.Buffer()
        var mat: Mat = input.clone()
	    def saveResult(processor: ImageProcessor[_, _], result: Any) {
	        results += ((processor, result))
	    }
        mutableHook = Some(saveResult)
        try {
            composed(mat)
        } finally {
            mutableHook = None
        }
        results
    }
    
    def updateImage {
        updateImage(orig, lastResults)
    }
    
    def updateImage(orig: Mat, results:Seq[(ImageProcessor[_, _], Any)]) {
        var image: Mat = if (drawOriginalCheck.selected) { orig.clone() } else Mat.zeros(orig.size(), CvType.CV_8UC3)
        for (pair <- results) {
            drawResult(pair._1, pair._2, image)
        }
        imageIcon.setImage(image)
        imageLabel.repaint()
    }
    
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
    schedule
}
