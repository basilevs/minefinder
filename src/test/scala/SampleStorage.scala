import org.scalatest.FunSuite
import java.awt.image.{BufferedImage}

import java.nio.file.Files.deleteIfExists
import java.nio.file.Paths

import minefinder.{Number, SampleStorage => Subject}
import minefinder.ImageTools._

class SampleStorageTest extends FunSuite {
	import minefinder.Sample
	val fields = Field.all
	test("different images are non-equal") {
		assert(compare(fields(0).image, fields(1).image)!=0)
	}
	test("same images are equal") {
		assert(compare(fields(0).image, fields(0).image)==0)
	}
	
	test("image serialization") {
		val image = fields(0).image
		val bytes = Subject.toBytes(image)
		val image2 = Subject.toImage(bytes)
		assert(compare(image, image2)==0)
	}
	
	test("save images") {
		deleteIfExists(Paths.get("testStorage.ser"))
		val storage1 = new Subject("testStorage")
		try {
			assert(storage1.size == 0)
			storage1 += new Sample(Number(2), fields(0).image)
			assert(storage1.size == 1)
			storage1.save
		} finally {
			storage1.clear //Prevent overwrite on finalization
		}
		val storage2 = new Subject("testStorage")
		try {
			assert(storage2.size == 1)
			assert(compare(storage2.toSeq(0)._2, fields(0).image)==0)
			assert(storage2.toSeq(0)._1 == Number(2))
		} finally {
			storage2.clear
			storage2.save
		}
		deleteIfExists(Paths.get("testStorage.ser"))
	}
}
