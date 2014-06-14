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
  
  /** Returns the begin and the end of the step with a given index */
  def getStep(index:Int): (Int, Int) = {
      (start + step*index, start + step*(index+1))
  }
  
  
  override def toString = {
    "Axis(%d, %d, %d, %d)".format(start, step, count, stop)
  } 
}