package recog

import org.opencv.core.Mat
import scala.collection.Seq
import org.opencv.imgproc.Imgproc

//class ContourProcessor extends ImageProcessor[Mat, CvSeq ] {
//
//  def name(): String = { "Contour" }
//
//  def parameters(): Seq =  Seq()
//  
//  override def draw(target: Mat, result: Any) {
//      
//  }
//
//  override def apply(input: Mat): CvSeq  = {
//      val rv = new CvSeq 
//      Imgproc.findContours(input, rv, hierarchy, mode, method)
//  }
//
//}