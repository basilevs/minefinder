package minefinder.grid

case class Axis(val start:Int, val step:Int, val count:Int) {
  assert(step > 0)
  assert(count > 0)
  val stop = start + step*count
  
  def ticks: Seq[Int] = start.until(stop+1, step)
  /** 
   *  Iterates over all cells providing index, start, end.
   */
  def forEachCell[T](f: (Int, Int, Int) => T): Seq[T] = {
    for (index <- 0 until count) yield {
      val start = step*index
      val stop = step * (index+1)
      f(index, start, stop)
    }
  }
}