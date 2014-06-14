package minefinder.field

trait Cell[+T] {
    def mark: T
    def neighboors: Set[_ <: Cell[T]]
}