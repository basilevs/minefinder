package recog

import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.TextField
import scala.swing.event.ValueChanged
import scala.swing.Label
import scala.runtime.RichDouble

class DoubleParameterComponent(val parameter: DoubleParameter) extends FlowPanel {
  val textField = new TextField
  textField.columns = 10
  textField.text = parameter.value.toString()
  textField.reactions += {
    case e: ValueChanged =>  { 
      try {
        parameter.value = textField.text.toDouble
		publish(e)
      } catch {
        case _ : NumberFormatException => 
      }
	}
  }
  contents += new Label(parameter.name + ": ")
  contents += textField
}