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
			xn <- math.max(x-1, 0) to math.min(x+1, columns-1);
			yn <- math.max(0, y-1) to math.min(y+1, rows-1);
			if (!(xn == x && yn == y))
		) yield {val pos = yn*columns + xn; cells(pos) } ).toSeq
		override def toString = "x:%d, y:%d, mark:%s".format(x, y, mark.getOrElse("None").toString)
	}
	def iterator = cells.iterator
}

object Field {
	def cellsToString(cells:Iterable[Cell]) = {
		val sb = new StringBuffer()
		for (c <- cells) {
			sb.append(c.toString+" ")
		}
		sb.toString
	}
	//Second element of tuple is true is mine is present in the cell
	def getCellsWithMineFlag(cells:Iterable[Cell]): Iterable[(Cell, Boolean)] = {
		cells.flatMap(c => c.mark match {
			case Some(Number(n)) => { //Mine or unknown count is exactly n
				val ns = c.neighbours
				val mines = ns.filter(_.mark == Some(Mine))
				val closed = ns.filter(_.mark == Some(Closed))
				//If there are exactly x yet undiscovered neighboring mines and x closed neighbors, they are the same   
				val rv1 = if ( (n - mines.size) == closed.size) {
					val rv = closed.map((_, true))
					if (rv.size > 0)
						println("Found all empties for cell "+cellsToString(Seq(c))+": "+rv)
					rv
				} else {
					Seq()
				}
				val rv2 = if (n==mines.size) {
					val rv = ns.filter(_.mark == Some(Closed)).map((_, false))
					if (rv.size > 0)
						println("Found all mines for cell "+cellsToString(Seq(c))+": "+rv)
					rv
				} else {
					Seq()
				}
				rv1 ++ rv2
			}
			case _ => Seq()
		})
	}
}