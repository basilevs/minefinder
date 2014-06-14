package minefinder.field

trait RectangularField[T] {
	def cells:Set[RectangularCell[T]]
	def get(x:Int, y:Int):RectangularCell[T]
}