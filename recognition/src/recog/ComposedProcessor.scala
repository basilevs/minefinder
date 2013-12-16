package recog

import org.opencv.core.Mat
import scala.collection.Seq

object ComposedProcessor {
  
  def findSecond[T, R](where:ImageProcessor[_,_], what: ImageProcessor[T, R]): Option[ComposedProcessor[_, T, R]] = {
    where match {
      case composed: ComposedProcessor[_, _, _] => {
        if (composed.second == what) {
          Some(where.asInstanceOf[ComposedProcessor[_, T, R]])
        } else {
        	findSecond(composed.first, what) orElse { findSecond(composed.second, what) }
        }
      }
      case _ => None
    }
  }
  
  def findFirst[T, R](where:ImageProcessor[_,_], what: ImageProcessor[T, R]): Option[ComposedProcessor[T, R, _]] = {
    where match {
      case composed: ComposedProcessor[_, _, _] => {
        if (composed.first == what) {
          Some(where.asInstanceOf[ComposedProcessor[T, R, _]])
        } else {
        	findFirst(composed.first, what) orElse { findFirst(composed.second, what) }
        }
      }
      case _ => None
    }
  }
  
  def safeCast[T](clazz:Class[T], input:Any): Option[T] = {
    if (clazz.isInstance(input))
      Some(input.asInstanceOf[T])
    else
      None
  }

}

class ComposedProcessor[-T, M, R] (val second:ImageProcessor[M, R], val first:ImageProcessor[T, M])
extends ImageProcessor[T, R] {
	import ComposedProcessor._
  def name(): String = second.name + " o " + first.name

  def parameters = second.parameters ++ first.parameters
  
  def draw(target: Mat, result: Any) {
	  second.draw(target, result.asInstanceOf[R])
  }
  
  override def apply(input:T, hook:Hook):R = {
    second.apply(first.apply(input, hook), hook)
  }
  
  def +[X](next:ImageProcessor[R, X]): ComposedProcessor[T, R, X] = {
    new ComposedProcessor(next, this)
  }
}
