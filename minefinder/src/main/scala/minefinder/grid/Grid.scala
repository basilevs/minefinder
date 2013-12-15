package minefinder.grid

case class Grid(val x: Axis, val y: Axis) {
  /**
   * Iterates over all cells providing x, y, left, top, right, bottom
   */
  def forEachCell[T](f: (Int, Int, Int, Int, Int, Int) => T): Seq[Seq[T]] = {
    val xAxis = x
    val yAxis = y

    xAxis.forEachCell {
      case (x, left, right) =>
        yAxis.forEachCell {
          case (y, top, bottom) =>
            f(x, y, left, top, right, bottom)
        }
    }
  }
}