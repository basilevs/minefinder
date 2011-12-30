package minefinder;
import collection.mutable.{HashSet, SetProxy}

import collection.JavaConversions._

import java.awt.image.{BufferedImage}
import java.io.{FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream, FileNotFoundException}

/**
 * Persistent sample storage for algorithm teaching.
 * Call clear to prevent ondisk storage overwrite on finalization.
 */
class SampleStorage(name:String) extends SetProxy[(Mark, BufferedImage)] {
	val self = new HashSet[(Mark, BufferedImage)]
	load
	def filename = name+".ser"
	def load {
		try {
			val fis = new FileInputStream(filename)
			val ois = new ObjectInputStream(fis)
			val data = ois.readObject().asInstanceOf[Set[(Mark, BufferedImage)]]
			self ++= data
		} catch {
			case e:FileNotFoundException =>
		}
	}
	def save {
		val oos = new ObjectOutputStream(new FileOutputStream(filename))
		oos.writeObject(self)
		oos.close()
	}
	override def finalize {
		if (self.size >0)
			save
	}
}

object SampleStorage {

}