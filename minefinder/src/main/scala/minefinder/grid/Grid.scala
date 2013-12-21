package minefinder.grid

case class Grid(val xAxis: Axis, val yAxis: Axis) {
    /**
     * Iterates over all cells providing x, y, left, top, right, bottom
     */
    def forEachCell[T](f: (Int, Int, Int, Int, Int, Int) => T): Seq[Seq[T]] = {
        xAxis.forEachCell {
            case (x, left, right) =>
                yAxis.forEachCell {
                    case (y, top, bottom) =>
                        f(x, y, left, top, right, bottom)
                }
        }
    }

    def getCell(xIndex: Int, yIndex: Int) = {
        val xStep = xAxis.getStep(xIndex)
        val yStep = yAxis.getStep(yIndex)
        (xStep._1, yStep._1, xStep._2, yStep._2)
    }
}