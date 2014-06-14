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
extends ImageProcessor[T, (M,R)] {
	import ComposedProcessor._
  def name(): String = second.name + " o " + first.name

  override def parameters = Seq() 
  
  override def draw(target: Mat, result: (M,R)) {
	  first.draw(target, result._1)
	  second.draw(target, result._2)
  }
  
  override def apply(input:T): (M, R) = {
    val f = first.apply(input)
    val s = second.apply(f)
    (f, s)
  }
  
  def +[X](next:ImageProcessor[R, X]): ComposedProcessor[T, (M,R), X] = {
    val wrapper = new ImageProcessor[(M,R), X] {
      override def name = "Wrapper("+next.name+")"
      override def parameters = Seq()
      override def draw(target: Mat, result: X) = next.draw(target, result)
      override def apply(input:(M,R)): X = next(input._2)
    }  
    new ComposedProcessor[T, (M,R), X](wrapper, this)
  }
}
