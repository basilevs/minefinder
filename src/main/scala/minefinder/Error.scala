package minefinder;
import java.awt.image.{BufferedImage}
import swing.{Button, Dialog, Action, BoxPanel, Orientation, Label}
import javax.swing.ImageIcon


class Error(reason:Error = null) extends RuntimeException(reason)

class RecognitionError(val problem:BufferedImage, reason:Error = null) extends Error(reason)
class ContradictoryRecognition(val matches:Seq[Sample], problem:BufferedImage, reason:Error = null) extends RecognitionError(problem, reason)

object Error {
	class SampleView(s:Sample) extends Label {
		icon = new ImageIcon(s.img)
		text = s.mark.toString
	}
	def handle(e:ContradictoryRecognition) {
		new Dialog() {
			title = "Contradictory recognition"
			modal = true
			contents = new BoxPanel(Orientation.Horizontal) {
				contents += new Label() {icon = new ImageIcon(e.problem); text = "To recognize"}
				contents ++= e.matches.map(new SampleView(_))
			}
			open
		}
	}
}