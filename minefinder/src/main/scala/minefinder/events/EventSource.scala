package minefinder.events

trait EventSource[T] {
	def +=(handler: (T) => Unit )
}