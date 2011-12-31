package minefinder;
import java.awt.image.{BufferedImage}

class Error(reason:Error = null) extends RuntimeException(reason)

class RecognitionError(problem:BufferedImage, reason:Error = null) extends Error(reason)
class ContradictoryRecognition(matches:Seq[Sample], problem:BufferedImage, reason:Error = null) extends RecognitionError(problem, reason)

object Error {
}