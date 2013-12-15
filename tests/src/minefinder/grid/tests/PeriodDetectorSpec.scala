package minefinder.grid.tests

import org.scalatest._
import org.scalatest.Matchers
import minefinder.grid.PeriodDetector
import minefinder.grid.Axis

class PeriodDetectorSpec extends FlatSpec with Matchers {

  "Horizontal axis" should " have exact parameters" in {
    val horizontalCoordinates = Set(0, 7, 9, 64, 67, 70, 98, 100, 132, 134, 166, 168, 188, 200, 202, 234, 236, 256, 268, 270, 302, 304, 324, 336, 338, 370, 372, 392, 404, 406, 438, 440, 472, 474, 506, 508, 540, 542, 574, 576, 597, 609, 611, 643, 645, 677, 679, 711, 713, 745, 747, 779, 781, 813, 815, 849, 881, 883, 917, 949, 951, 983, 985, 1017, 1019, 1051, 1053, 1085, 1087, 1141, 1143, 1149)
    val pd = new PeriodDetector(1, 1, 30)
    assert(pd(horizontalCoordinates) == Some(Axis(65, 34, 30)))
  }

  "Vertical axis" should " have exact parameters" in {
    val horizontalCoordinates = Set(0, 28, 30, 105, 108, 111, 112, 139, 141, 142, 173, 175, 176, 207, 210, 241, 243, 275, 278, 309, 312, 343, 346, 377, 379, 380, 412, 414, 443, 446, 449, 480, 483, 514, 517, 548, 550, 582, 585, 616, 619, 650, 652, 712, 714, 715, 720)
    val pd = new PeriodDetector(1, 1, 30)
    assert(pd(horizontalCoordinates) == Some(Axis(107, 34, 16)))
  }
}
