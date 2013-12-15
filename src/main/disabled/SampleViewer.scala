package minefinder
import scala.swing._

class SampleViewer extends MainFrame {
	val mode = new FlowPanel() {
		def createButton(mark:Mark) = {
			new RadioButton() {
				action = new Action(mark.toString){def apply {view.fill(mark)}}
			}
		}
		val btns:Array[AbstractButton] = Mark.all.map(createButton).toArray
		contents ++= btns
		val group = new ButtonGroup(btns:_*)
		preferredSize = new Dimension(btns.map(_.maximumSize.getWidth()).sum.toInt, btns(0).maximumSize.getHeight.toInt)
		maximumSize = preferredSize 
	}
	val view = new FlowPanel() {
		def sampleToIcon(s:Sample) = { new Label() {
			icon=new javax.swing.ImageIcon(s.img)
			
		}}
		def fill(mark:Mark) {
			contents.clear
			val data = SampleStorage.instance.filter(_.mark == mark)
			contents += new Label{text = "%d %s samples: ".format(data.size, mark.toString)}  
			contents ++= data.map(sampleToIcon)
			revalidate
			repaint
		}
	}
	contents = new BoxPanel(Orientation.Vertical) {contents++=Seq(mode,view)}
}

object SampleViewer extends SimpleSwingApplication {
	def top = new SampleViewer()
}