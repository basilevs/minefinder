package minefinder;
import collection.mutable.{HashSet, SetProxy, Set}

import collection.JavaConversions._

import java.awt.image.{BufferedImage}
import javax.imageio.ImageIO
import java.io.{FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream, FileNotFoundException, EOFException, ByteArrayOutputStream, ByteArrayInputStream}

/**
 * Persistent sample storage for algorithm teaching.
 * Call clear to prevent ondisk storage overwrite on finalization.
 */
class SampleStorage(name:String) extends SetProxy[(Mark, BufferedImage)] {
	val self = new HashSet[(Mark, BufferedImage)]
	val difcalc = new Recognizer.ColorDifference(2) 
	def filename = name+".ser"
	import SampleStorage._
	load
	override def += (pair: (Mark, BufferedImage)): SampleStorage.this.type = {
		assert(pair._2 != null)
		for(present <- self) {
			assert(present._2 != null)
			if (difcalc.difference(present._2, pair._2) < 0.0001) {
				if (present._1 != pair._1) {
					throw new RuntimeException("Can't store contradictory pattern.")
				}
//				println("Refusing storing/loading dublicated pattern")
				return this
			}
		}
		self += pair
		this
	}
	def load {
		self.clear
		try {
			val fis = new FileInputStream(filename)
			val ois = new ObjectInputStream(fis)
			try {
				while(true) {
					val data = ois.readObject().asInstanceOf[(Mark, Array[Byte])]
					this += ((data._1, toImage(data._2)))
				}
			} catch {
				case e:EOFException =>
			} finally {
				ois.close
				fis.close
			}
		} catch {
			case e:FileNotFoundException =>
		}
	}
	def save {
		val oos = new ObjectOutputStream(new FileOutputStream(filename))
		for (pair <- self) {
			oos.writeObject((pair._1, toBytes(pair._2)))
		}
		oos.close()
	}
}

object SampleStorage {
	val instance = new SampleStorage("samples")
	def toBytes(img:BufferedImage):Array[Byte] = {
		val baos = new ByteArrayOutputStream();
		ImageIO.write(img, "PNG", baos);
		baos.toByteArray();
	}
	def toImage(bytes:Array[Byte]):BufferedImage = {
		val bais = new ByteArrayInputStream(bytes)
		val rv = ImageIO.read(bais)
		assert(rv != null)
		rv
	}
}