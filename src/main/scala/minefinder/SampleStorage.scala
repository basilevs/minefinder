package minefinder;
import collection.mutable.{Buffer}

import collection.JavaConversions._

import java.awt.image.{BufferedImage}
import javax.imageio.ImageIO
import java.io.{FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream, FileNotFoundException, EOFException, ByteArrayOutputStream, ByteArrayInputStream}

/**
 * Persistent sample storage for algorithm teaching.
 * Call clear to prevent ondisk storage overwrite on finalization.
 */
class SampleStorage(name:String) extends collection.mutable.Set[Sample] {
	private val self = Buffer[Sample]()
	val listeners = Buffer[Sample => Unit]()
	def filename = name+".ser"
	import SampleStorage._
	load
	def find(s:Sample):Option[Sample] = {
		self.find(pair=>ImageTools.differencePerPixel(s.img, pair.img) < 2)
	}
	def contains(s:Sample):Boolean = {
		val res = find(s)
		!res.isEmpty && res.get.mark == s.mark
	}
	def iterator = self.iterator
	def += (pair: Sample): SampleStorage.this.type = {
		assert(pair._2 != null)
		val res = find(pair)
		if (!res.isEmpty) {
			if(res.get.mark != pair.mark)
				throw new TrainConflict(Seq(res.get), pair)
		}
		self += pair
		listeners.foreach(_(pair))
		this
	}
	def -= (pair: Sample):SampleStorage.this.type = {
		assert(pair.img != null)
		find(pair).foreach(self.-=)
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
					self += Sample(data._1, toImage(data._2))
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