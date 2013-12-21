package minefinder.events
import collection.mutable

class EventBus[T] extends EventSource[T] with Function1[T, Unit] {
    private val reactions: mutable.Set[Function1[T, Unit]] = mutable.Set()
    override def +=(handler: Function1[T, Unit]): Unit = reactions += handler
    override def apply(event:T) {
        reactions.foreach(_(event))
    }
}