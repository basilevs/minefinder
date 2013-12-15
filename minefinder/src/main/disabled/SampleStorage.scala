package minefinder;
import collection.mutable.{Buffer}
import collection.JavaConversions._
import java.awt.image.{BufferedImage}
import javax.imageio.ImageIO
import java.io.{FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream, FileNotFoundException, EOFException, ByteArrayOutputStream, ByteArrayInputStream}
import java.nio.file.Paths
import OnceCloseable._
import java.io.InputStream

/* Persistent storage of samples. */
class SampleStorage(name:String) {
	import SampleStorage._
	def filename = name+".ser"
	def save(data:Iterator[Sample]) {
		def filename = name+".ser"
		val oos = new ObjectOutputStream(new FileOutputStream(filename))
		for (pair <- data) {
			oos.writeObject((pair.mark, toBytes(pair.img)))
		}
		oos.close()
	}
	private def loadFromStream(s:InputStream):Iterator[Sample] with AutoCloseable = {
			new Iterator[Sample] with AutoCloseable {
				val ois = new ObjectInputStream(s)
				println("Opened: "+Paths.get(filename).toAbsolutePath)
				var count = 0
				def getNext = {
					try {
						val data = ois.readObject().asInstanceOf[(Mark, Array[Byte])]
						count += 1
						new Sample(data._1, toImage(data._2))
					} catch {
						case e:EOFException => {
							println("Read %d samples from %s".format(count, filename))
							null
						}
					}
				}
				def close {
					ois.close
					s.close
				}
				override def finalize {close}
				var _next = getNext
				def next = {val rv = _next; _next = getNext; rv}
				def hasNext = _next != null 
			}
	} 
	private def tryOpenFile:Option[InputStream] = {
		try {
			Option(new FileInputStream(filename))
		} catch {
			case e:FileNotFoundException => None
		}
	}
	private def tryOpenResource:Option[InputStream] = {
		try {
			val res = classOf[SampleStorage].getResourceAsStream(filename)
			Option(new FileInputStream(filename))
		} catch {
			case e:FileNotFoundException => None
		}
	}
	def empty = new Iterator[Sample]() with AutoCloseable {def hasNext = false; def next = null; def close {}}
	def load:Iterator[Sample] with AutoCloseable = {
		try {
			tryOpenFile.orElse(tryOpenResource).map(loadFromStream).getOrElse(empty)
			
		} catch {
			case e:FileNotFoundException => empty
		}
	}
}
/**
 * Persistent sample storage for algorithm teaching.
 * Controls uniqueness.
 */
class UniqSampleStorage(name:String) extends collection.mutable.Set[Sample] {
	private val self = Buffer[Sample]()
	private val storage = new SampleStorage(name)
	val listeners = Buffer[Sample => Unit]()
	import SampleStorage._
	load
	def find(img:BufferedImage):Option[Sample] = {
		self.find(pair=>ImageTools.differencePerPixel(img, pair.img) < 2)
	}
	def contains(s:Sample):Boolean = {
		val res = find(s.img)
		!res.isEmpty && res.get.mark == s.mark
	}
	def iterator = self.iterator
	def += (pair: Sample): UniqSampleStorage.this.type = {
		assert(pair._2 != null)
		val res = find(pair.img)
		if (!res.isEmpty) {
			if(res.get.mark != pair.mark)
				throw new TrainConflict(Seq(res.get), pair)
		}
		self += pair
		listeners.foreach(_(pair))
		this
	}
	def -= (pair: Sample):UniqSampleStorage.this.type = {
		assert(pair.img != null)
		find(pair.img).foreach(self.-=)
		this
	}
	def load {
		self.clear
		tryWith(storage.load){self ++= _} 
	}
	def save {
		storage.save(self.iterator)
	}
}

object SampleStorage {
	val instance = new UniqSampleStorage("samples")
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