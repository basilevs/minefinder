package minefinder.grid.tests

import org.scalatest._
import org.scalatest.Matchers
import minefinder.grid.Axis
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import minefinder.grid.AxisWeightProvider

@RunWith(classOf[JUnitRunner])
class AxisWeightProviderSpec extends FlatSpec with Matchers {
  val data1 = Seq(7, 10, 13, 41, 43, 75, 97, 109, 111, 113, 131, 143, 165, 177, 179, 199, 211, 232, 245, 247, 279, 301, 313, 315, 337, 347, 368, 381, 383, 417, 437, 448, 449, 451, 469, 471, 483, 505, 517, 519, 520, 540, 552, 554, 586, 588, 619, 620, 622, 640, 654, 656, 688, 690, 722, 724, 756, 758, 792, 824, 826, 860, 892, 894, 928, 960, 962, 994, 996, 1028, 1030)
  val cleanData2 = Seq(7, 10, 41, 43, 75, 109, 111, 143, 177, 179, 211, 245, 247, 279, 313, 315, 347, 381, 383, 417, 448, 449, 451, 483, 517, 519, 520, 552, 554, 586, 588, 619, 620, 622, 654, 656, 688, 690, 722, 724, 756, 758, 792, 824, 826, 860, 892, 894, 928, 960, 962, 994, 996, 1028, 1030)
  val data3 = Seq(9, 25, 43, 60, 62, 81, 97, 117, 133, 143, 151, 161, 164, 169, 181, 187, 188, 205, 223, 259, 296, 331, 368, 388, 403, 439, 475, 496, 511, 547)
  val data4 = Seq(32, 35, 64, 69, 70, 98, 107, 165, 169, 171, 200, 203, 268, 273, 307, 308, 340, 342, 343, 347, 371, 376, 410, 411, 416, 441, 442, 512, 580)

  "Axis with more ticks" should " be better" in {
    val avp = new AxisWeightProvider(cleanData2.toSet, 1, 1, 0.2)
    val nineTicksScore = avp.calcScore(new Axis(8, 34, 9), 100)
    val thirtyTicksScore = avp.calcScore(new Axis(8, 34, 30), 100)
    assert(nineTicksScore < thirtyTicksScore)
  }

  "One tick " can " be skipped" in {
    val avp = new AxisWeightProvider(data3.toSet, 1, 2, 0.2)
    val nineteenTicksScore = avp.calcScore(new Axis(9, 18, 28), 100)
    val thirtyTicksScore = avp.calcScore(Axis(9, 18, 30), 100)
    assert(nineteenTicksScore < thirtyTicksScore)
  }

  "There " should " be thirty ticks" in {
    val avp = new AxisWeightProvider(data3.toSet, 1, 2, 0.2)
    val rv = avp.findBestWithWidth(18, 100)
    assert(rv.isDefined)
    assert(rv.get._1.count == 30)
  }
  
  "There " should " be 16 ticks" in {
    val avp = new AxisWeightProvider(data4.toSet, 1, 2, 0.2)
    val rv = avp.findBestWithWidth(34, 100)
    assert(rv.isDefined)
    assert(rv.get._1.count == 16)
  }
  
  "Axis with 16 ticks" should " be better" in {
    val avp = new AxisWeightProvider(data4.toSet, 1, 2, 0.2)
    val twelveTicksScore = avp.calcScore(new Axis(34, 34, 12), 100)
    val sixteenTicksScore = avp.calcScore(new Axis(34, 34, 16), 100)
    assert(twelveTicksScore < sixteenTicksScore)
  }

  
}