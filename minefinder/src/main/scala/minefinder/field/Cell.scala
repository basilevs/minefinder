package minefinder.field

trait Cell {
    def mark: Option[Mark]
    def neighboors: Set[Cell]
}