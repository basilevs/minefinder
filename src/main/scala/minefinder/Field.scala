package minefinder;

import math.abs
import swing._

trait Cell {
	def x:Int
	def y:Int
	def neighbours: Iterable[Cell]
	def mark:Option[Mark]
	def mayHaveMine = {
		val m= mark
		!m.getOrElse(Mine).isInstanceOf[Number]
	}
	override def equals(that:Any) = that match {
	  case c:Cell => (x == c.x) && (y == c.y)
	  case _ => false
	}
	override def hashCode = (1000 * y) + x
}

class Field(val columns:Int, marks:Seq[RecognitionResult]) extends Iterable[Cell] {
	val rows = marks.size/columns
	val cells = marks.zipWithIndex.map( p => new CellI(p._1, p._2)) 
	class CellI(val result:RecognitionResult, pos:Int) extends Cell {
		val x = pos % columns
		val y = pos / columns
		def mark = result.result
		def neighbours = (for (
			xn <- math.max(x-1, 0) to math.min(x+1, columns-1);
			yn <- math.max(0, y-1) to math.min(y+1, rows-1);
			if (!(xn == x && yn == y))
		) yield {val pos = yn*columns + xn; cells(pos) } ).toSeq
		override def toString = "x:%d, y:%d, mark:%s".format(x, y, mark.getOrElse("None").toString)
	}
	def getRow(y:Int) = marks.slice(columns*y, columns*(y+1))
	def iterator = cells.iterator
}

class FieldView(field:Field) extends Frame {
	val defaultMargin = new Insets(1, 1, 1, 1)
	class RecognitionButton(r:RecognitionResult) extends Button {
		maximumSize = new Dimension(20, 20)
		text = r.result match {
			case Some(Number(n)) => n.toString
			case Some(Mine) => "m"
			case Some(Question) => "?"
			case Some(Closed) => "c"
			case None => "u"
		}
		action = new Action(text) {
			def apply {
				val v = r.getView
				v.open
			}
		}
		margin = defaultMargin
	}
	class Row(data:Seq[RecognitionResult]) extends BoxPanel(Orientation.Horizontal) {
		contents ++= data.map(new RecognitionButton(_))
	}
	
	contents = new BoxPanel(Orientation.Vertical) {
		contents ++= 0.until(field.rows).map(idx => new Row(field.getRow(idx)))
	} 
	
}

object Field {
	def cellsToString(cells:Iterable[Cell]) = {
		val sb = new StringBuffer()
		for (c <- cells) {
			sb.append(c.toString+" ")
		}
		sb.toString
	}
	def isClosed(c:Cell) = {
		if (c.mark.isEmpty) {
			true
		} else if (c.mark.get.isInstanceOf[Number] || c.mark.get == Mine) {
		  false
		} else {
		  true
		}
	}
	def isNumber(c:Cell) = c.mark.getOrElse(Question).isInstanceOf[Number]
	def inspectPair(a:Cell, b:Cell): Iterable[(Cell, Boolean)] = {
		val seeds = Seq(a, b)
		if (a == b || seeds.exists(!isNumber(_))){
			Seq()
		} else {
			val ns = seeds.map(_.neighbours.toSet)
			val closed = ns.map(_.filter(isClosed))
			val mines = ns.map(_.filter(_.mark == Option(Mine)))
			val numbers = seeds.map(_.mark.get.asInstanceOf[Number].n)
			val aDiff = numbers(0) - numbers(1) - mines(0).size + mines(1).size
			val onlyA = closed(0) -- closed(1)
			val onlyB = closed(1) -- closed(0)
			if ((onlyA.size == abs(aDiff)) && (onlyA.size == onlyB.size || onlyB.size == 0)){
				onlyA.filter(_.mark == Some(Closed)).map((_, aDiff>0))
			} else {
				Seq()
			}
		}
	}
	//Second element of tuple is true is mine is present in the cell
	def getCellsWithMineFlag(cells:Iterable[Cell]): Iterable[(Cell, Boolean)] = {
		cells.flatMap(c => c.mark match {
			case Some(Number(n)) => { //Mine or unknown count is exactly n
				val ns = c.neighbours
				val mines = ns.filter(_.mark == Some(Mine))
				val closed = ns.filter(isClosed)
				val undetected = n - mines.size
				if (ns.exists(_.mark.isEmpty)) {
				  Seq()
				} else if (closed.size == 0) {
				  Seq()
				} else if ( undetected == closed.size) {
				//If there are exactly x yet undiscovered neighboring mines and x closed neighbors, they are the same   
					val rv = closed.filter(_.mark == Some(Closed)).map((_, true))
					if (rv.size > 0)
						println("Found all empties for cell "+cellsToString(Seq(c))+": "+rv)
					rv
				} else if (n == mines.size) {
					val rv = ns.filter(_.mark == Some(Closed)).map((_, false))
					if (rv.size > 0)
						println("Found all mines for cell "+cellsToString(Seq(c))+": "+rv)
					rv
				} else {
					def inspectThatAsPair(x:Cell) = inspectPair(c, x)
					val rv = ns.map(_.neighbours.map(inspectThatAsPair).flatten).flatten.toSet
					if (rv.size>0)
						println("Found pairing for cell "+cellsToString(Seq(c))+": "+rv)
					rv
				}
			}
			case _ => Seq()
		}).toSet
	}
}