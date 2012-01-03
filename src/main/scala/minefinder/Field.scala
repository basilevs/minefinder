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
	}
	def iterator = cells.iterator
}

object Field {
	def getEmptyClosedCells(cells:Iterable[Cell]) = {
		cells.flatMap(c => c.mark match {
			case Some(Number(n)) => {
				val ns = c.neighbours
				if(n==(ns.count(_.mayHaveMine)))
					ns.filter(!_.mayHaveMine)
				else
					Seq()
			}
			case _ => Seq()
		})
	}
}