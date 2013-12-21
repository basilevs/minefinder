package minefinder.field
import collection.mutable.Set

trait Field {
    def cells: Set[Cell]
    val reactions:Set[(Cell) => Unit] = Set()    
}