package minefinder;
import java.awt.image.{BufferedImage}
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label, Component}
import javax.swing.ImageIcon


class Error(reason:Error = null) extends RuntimeException(reason)
class UserFail extends Error

class RecognitionError(val problem:BufferedImage, reason:Error = null) extends Error(reason)
class ContradictoryRecognition(val matches:Seq[Sample], problem:BufferedImage, reason:Error = null) extends RecognitionError(problem, reason)

object Error {
	class SampleView(s:Sample) extends Label {
		icon = new ImageIcon(s.img)
		text = s.mark.toString
	}
	def possibleMatches(matches:Iterable[(Sample, Float)], problem: BufferedImage) = {
		new BoxPanel(Orientation.Horizontal) {
			contents += new Label() {icon = new ImageIcon(problem); text = "Unrecognized"}
			contents ++= matches.map(x => {
				val (s,d) = x
				val q = new SampleView(s)
				q.text += ", delta: "+ d
				q
			})
		}
	}
	def contradictoriesComponent(matches:Iterable[Sample], problem:BufferedImage) = {
		new BoxPanel(Orientation.Horizontal) {
			contents += new Label() {icon = new ImageIcon(problem); text = "Unrecognized"}
			contents ++= matches.map(new SampleView(_))
		}
	}
	def recognitionComponent(problem:BufferedImage) = {
		new Label() {icon = new ImageIcon(problem); text = "Unrecognized"}
	}
	def showComponent(c:Component, wait:Boolean = true, title:String = "") {
		val waitForDialog = wait
		val titleForDialog = title
		new Dialog() {
			title = titleForDialog
			modal = waitForDialog
			contents = c
			open
		}
	}
	def show(er:RecognitionError, wait:Boolean=false) {
			val c = er match {
				case e:ContradictoryRecognition => contradictoriesComponent(e.matches, e.problem)
				case e:RecognitionError => recognitionComponent(e.problem)
			}
			showComponent(c, wait)
	}
	def handle(e:RecognitionError) {
		show(e, true)
	}
}