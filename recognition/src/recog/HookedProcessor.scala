package recog

import org.opencv.core.Mat

class HookedProcessor[-T, R](val delegate: ImageProcessor[T, R], val hook: (ImageProcessor[_, _], Any) => Unit) extends ImageProcessor[T, R] {
    def name = delegate.name
    def parameters: Seq[DoubleParameter] = delegate.parameters
    def draw(target: Mat, result: Any) = delegate.draw(target, result)
    def apply(input: T): R = {
        val rv = delegate(input)
        hook(delegate, rv)
        rv
    }

}