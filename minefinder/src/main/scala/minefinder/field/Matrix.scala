package minefinder.field

abstract class Matrix[T](val width:Int, val height:Int) extends Function2[Int, Int, T] {
  private val matrix:Array[Array[Any]] = Array.ofDim(width, height)
  @inline private def validatePos (x: Int, y:Int) {
	    assert(x>=0)
	    assert(y>=0)
	    assert(x<width)
	    assert(y<height)
	}
 
  for (x <- 0 until width; y <- 0 until height) {
    matrix(x)(y) = initializeValue(x, y)
  }

  override def apply(x: Int, y: Int): T = {
    validatePos(x, y)
    matrix(x)(y).asInstanceOf[T]
  }
  
  def initializeValue(x:Int, y:Int): T

  def neighboorCoordinates(x0: Int, y0: Int) = {
       for(
        		x <- (x0-1) until (x0+1);
        		y <- (y0-1) until (y0+1);
        		if (!(x == x0 && y == y0));
        		if (x>=0 && y>=0 && x<width && y<height)
       ) yield (x, y)
  }
  
  def contents:Set[T] = {
    {for (x <- 0 until width; y <- 0 until height)yield matrix(x)(y).asInstanceOf[T]}.toSet
  }
  
  def neighboors(x0:Int, y0:Int):Set[T] = {
    neighboorCoordinates(x0, y0).map( (coords:(Int, Int)) => apply(coords._1,coords._2)).toSet
  }
}