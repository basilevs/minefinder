package minefinder.field;

abstract class LazyField[T](val width: Int, val height: Int) extends RectangularField[T] {
  private class StatefulCellInternal(val x: Int, val y: Int) extends RectangularCell[T] {
    var payload: T = defaultValue
    override def mark = {
      payload = calculateValue(x, y, payload)
      payload
    }
    override lazy val neighboors: Set[_ <: Cell[T]] = matrix.neighboors(x, y)
  }

  def calculateValue(x: Int, y: Int, oldValue: T): T
  def defaultValue: T

  private val matrix = new Matrix[StatefulCellInternal](width, height) {
    override def initializeValue(x: Int, y: Int) = new StatefulCellInternal(x, y)
  }
  
  override def get(x: Int, y: Int): RectangularCell[T] = {
    matrix(x, y)
  }

  /** @return cell coordinates tuple (x,y)*/
  def unpack(cell: Cell[T]) = {
    val sc = cell.asInstanceOf[StatefulCellInternal]
    (sc.x, sc.y)
  }
}