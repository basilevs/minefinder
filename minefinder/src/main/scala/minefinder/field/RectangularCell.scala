package minefinder.field

trait RectangularCell[+T] extends Cell[T] {
    def x: Int
    def y: Int
}