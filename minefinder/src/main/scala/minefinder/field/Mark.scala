package minefinder.field;

trait Mark {
	def clickAllowed: Boolean
	def known: Boolean 
}

case class Number(n:Int) extends Mark {
	def clickAllowed = false
	assert(n >= 0)
	assert(n <= 8)
	override def toString = n.toString
	override def known = true
}

case object Closed extends Mark {
	def clickAllowed = true
	override def toString = "Closed"
	override def known = false
}

case object Question extends Mark {
	def clickAllowed = true
	override def toString = "Question"
   	override def known = false
}

case object Mine extends Mark {
	def clickAllowed = false
	override def toString = "Mine"
   	override def known = true
}

object Mark {
	val all = Seq[Mark](
		Number(0),
		Number(1),
		Number(2),
		Number(3),
		Number(4),
		Number(5),
		Number(6),
		Number(7),
		Number(8),
		Question,
		Mine,
		Closed
	)
}