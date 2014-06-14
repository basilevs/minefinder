package recog

import org.opencv.core.Mat

class ForwardingProcessor[-T, R](val delegate: ImageProcessor[T, R]) extends ImageProcessor[T, R] {
    def name = delegate.name
    def parameters: Seq[DoubleParameter] = delegate.parameters
    def draw(target: Mat, result: R) = delegate.draw(target, result)
    def apply(input: T): R = delegate(input)
}