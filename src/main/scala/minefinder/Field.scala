package minefinder;

trait Cell {
	def x:Int
	def y:Int
	def neighbours: Iterable[Cell]
	def mark:Option[Mark]
	def mayHaveMine = {
		val m= mark
		!m.getOrElse(Mine).isInstanceOf[Number]
	}
}

class Field(val columns:Int, marks:Seq[Option[Mark]]) extends Iterable[Cell] {
	val rows = marks.size/columns
	val cells = marks.zipWithIndex.map( p => new CellI(p._1, p._2)) 
	class CellI(val mark:Option[Mark], pos:Int) extends Cell {
		val x = pos % columns
		val y = pos / columns
		def neighbours = (for (
			xn <- math.max(x-1, 0) to math.min(x+1, columns);
			yn <- math.max(0, y-1) to math.min(y+1, rows);
			if (xn != x && yn != y)
		) yield {val pos = yn*columns + xn; cells(pos) } ).toSeq
		override def toString = "x:%d, y:%d, mark:%s".format(x, y, mark.getOrElse("None").toString)
	}
	def iterator = cells.iterator
}

object Field {
	def cellsToString(cells:Iterable[Cell]) {
		val sb = new StringBuffer()
		for (c <- cells) {
			sb.append(c.toString+" ")
		}
		sb.toString
	}
	def getCellsWithMineFlag(cells:Iterable[Cell]) = {
		cells.flatMap(c => c.mark match {
			case Some(Number(n)) => { //Mine or unknown count is exactly n
				val ns = c.neighbours
				
				val rv1 = if(n==(ns.count(_.mayHaveMine))) {
					println("Found all empties for cell "+cellsToString(Seq(c)))
					ns.filter(_.mark == Closed).map((_, true))
				} else {
					Seq()
				}
				val rv2 = if (n==(ns.count(_.mark == Mine))) {
					println("Found all mines for cell "+cellsToString(Seq(c)))
					ns.filter(_.mark != Mine).map((_, false))
				} else {
					Seq()
				}
				rv2
			}
			case _ => Seq()
		})
	}
}