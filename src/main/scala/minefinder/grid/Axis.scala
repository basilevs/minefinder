package minefinder.grid

case class Axis(val start:Int, val step:Int, val count:Int) {
  assert(step > 0)
  assert(count > 0)
  val stop = start + step*count
  
  def ticks: Seq[Int] = start.until(stop+1, step)
}