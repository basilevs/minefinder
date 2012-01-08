package minefinder;
import java.awt.image.{BufferedImage}
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label, Component}
import javax.swing.ImageIcon


class Error(reason:Error = null) extends RuntimeException(reason)

class UserFail extends Error

class RecognitionError(val problem:BufferedImage, reason:Error = null) extends Error(reason)
class ContradictoryRecognition(val matches:Seq[Sample], problem:BufferedImage, reason:Error = null) extends RecognitionError(problem, reason)
class TrainConflict(matches:Seq[Sample], problem:Sample, reason:Error = null) extends ContradictoryRecognition(matches, problem.img, reason)
class ListDialog extends Dialog {
	val p = new BoxPanel(Orientation.Vertical)
	contents = p
	val items = p.contents
	modal = false
	def add(c:Component) = {
		items += c
		if (items.size < 10)
			p.revalidate
	}
}

trait OnceCloseable extends  AutoCloseable {
	def closeOnce
	private var closed = false
	final def close {
		if (!closed) {
			closeOnce
			closed = true
		}
	}
	override def finalize {
		close
	}
}

object OnceCloseable {
	def tryWith[A<%AutoCloseable, R](ac: A)(f: A => R):R =
	{
		try {
			f(ac)
		} finally {
			ac.close
		}
	}
	def tryWith[A1<%AutoCloseable, A2<%AutoCloseable, R](ac1: A1, ac2:A2)(f: (A1,A2) => R):R = {
		tryWith(ac1) { r1 => tryWith(ac2) {r2 => f(r1, r2)}}
	}
}


object Error {
	class SampleView(s:Sample) extends Label {
		icon = new ImageIcon(s.img)
		text = "%10s".format(s.mark.toString)
	}
	def possibleMatches(matches:Iterable[(Sample, Float)], problem: BufferedImage) = {
		new BoxPanel(Orientation.Horizontal) {
			contents += new Label() {icon = new ImageIcon(problem); text = "Unrecognized"}
			contents ++= matches.map(x => {
				val (s,d) = x
				val q = new SampleView(s)
				q.text += ", delta: %.2f".format(d)
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
	def showRecognitionError(er:RecognitionError, wait:Boolean=false) {
			val c = er match {
				case e:ContradictoryRecognition => contradictoriesComponent(e.matches, e.problem)
				case e:RecognitionError => recognitionComponent(e.problem)
			}
			showComponent(c, wait)
	}
	def handle(e:RecognitionError) {
		showRecognitionError(e, true)
	}
}