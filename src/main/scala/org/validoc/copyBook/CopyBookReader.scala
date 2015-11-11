package org.validoc.copyBook

import java.nio.charset.Charset
import java.io.FileInputStream
import play.api.libs.functional.FunctionalCanBuild
import play.api.libs.functional.FunctionalBuilderOps
import java.io.InputStream
import play.api.libs.functional.Functor
import java.io.PrintWriter
import java.io.FileOutputStream

class EndOfStreamException extends Exception

class CopyBookStream(inputStream: InputStream) {
  var firstRead: Boolean = true
  var hasMoreData: Boolean = true
  def read(bytes: Array[Byte]) = { // The problem this is solving is 'how do I know I am at the end of file' 
    if (hasMoreData) {
      val l = inputStream.read(bytes)
      if (l == -1) {
        if (firstRead)
          hasMoreData = false
        else throw new EndOfStreamException
      }
    }
  }
  def as[X](implicit reader: CopyBookReader[X]) = reader.apply(this)
}

trait CopyBookReader[X] {
  def apply(implicit copyBookStream: CopyBookStream): X
  def map[Y](f: X => Y) = FunctorCopyBookReader(this, f)
}

trait FixedLengthCopyBookReader[X] extends CopyBookReader[X] {
  val buffer = new Array[Byte](length)
  val default: X
  def apply(implicit copyBookStream: CopyBookStream): X = {
    val bytes = copyBookStream.read(buffer)
    //    print(s"${getClass.getSimpleName}($length) bytes: ${buffer.map(b => Integer.toHexString(b.toInt)).toList}")
    if (copyBookStream.hasMoreData)
      reads(buffer)
    else
      default
  }
  def reads(bytes: Array[Byte]): X
  def length: Int
}

case class FunctorCopyBookReader[X, Y](reader: CopyBookReader[X], mapFn: X => Y) extends CopyBookReader[Y] {
  def apply(implicit copyBookStream: CopyBookStream) = {
    mapFn(reader.apply(copyBookStream))
  }
}

case class ListCopyBookReader[X](repeatLength: Int, listReader: CopyBookReader[X]) extends CopyBookReader[List[X]] {
  def repeatCountReader = CompCopyBookReader(repeatLength)
  def apply(implicit copyBookStream: CopyBookStream) = {
    val count = repeatCountReader.apply
    (1 to count).map(i => listReader.apply).toList
  }

}
case class StringCopyBookReader[X](val length: Int, trimIt: Boolean = true) extends FixedLengthCopyBookReader[String] {
  val default = ""
  def reads(bytes: Array[Byte]) = {
    val result = new String(bytes, CopyBookReader.charSet)
    //    println("=>[" + result + "]")
    if (trimIt) result.trim else result
  }
}
case class CompCopyBookReader[X](compLength: Int) extends FixedLengthCopyBookReader[Int] {
  lazy val length = compLength / 2
  if (length % 2 != 0) throw new RuntimeException("Not supported")
  val default = 0
  def reads(bytes: Array[Byte]) = {
    val result = bytes.foldLeft(0)((acc, v) => acc * 256 + (v & 0xff))
    //    println(" => " + result)
    result
  }
}
case class NumCopyBookReader[X](length: Int) extends FixedLengthCopyBookReader[Int] {
  val default = 0
  def reads(bytes: Array[Byte]) = {
    val result = bytes.foldLeft(0)((acc, v) =>
      acc * 10 + (v & 0x0f))
    //    println(" => " + result)
    result
  }
}

object CopyBookReader {
  val charSet = Charset.forName("cp1047")
  implicit object FuncBuilder extends FunctionalCanBuild[CopyBookReader] {
    import play.api.libs.functional.~
    import scala.language.higherKinds

    def apply[A, B](ma: CopyBookReader[A], mb: CopyBookReader[B]) = new CopyBookReader[A ~ B] {
      def apply(implicit stream: CopyBookStream) = new ~(ma.apply(stream), mb.apply(stream))
    }
  }
  implicit object FunctorCopyReader extends Functor[CopyBookReader] {
    def fmap[A, B](m: CopyBookReader[A], f: A => B) = new FunctorCopyBookReader[A, B](m, f)
  }
  import scala.language.implicitConversions

  implicit def toOps[X](x: CopyBookReader[X]) = new FunctionalBuilderOps[CopyBookReader, X](x)

  def Num(length: Int) = NumCopyBookReader(length)
  def C(length: Int) = CompCopyBookReader(length)
  def X(length: Int) = StringCopyBookReader(length)
  def list[X](repeatLength: Int)(implicit reader: CopyBookReader[X]) = ListCopyBookReader(repeatLength, reader)

  def foldLeft[Acc, X](file: String)(initial: Acc)(foldFn: (Acc, X) => Acc)(implicit reader: CopyBookReader[X]): Acc = {
    val inputStream = new FileInputStream(file)
    var acc = initial
    try {
      implicit val stream = new CopyBookStream(inputStream)
      while (stream.hasMoreData) {
        stream.firstRead = true
        val next = reader.apply
        acc = foldFn(acc, next)
      }
      acc
    } finally { inputStream.close }
  }

  def iterable[Acc, X](file: String)(implicit reader: CopyBookReader[X]) = new Iterable[X] {
    def iterator = new Iterator[X] {
    	val inputStream = new FileInputStream(file)
      implicit val stream = new CopyBookStream(inputStream)
      def hasNext = stream.hasMoreData
      def next = {
        stream.firstRead = true
        val result = reader.apply
        if (!hasNext) inputStream.close // and this sucks... 
        result
      }
      override def finalize = inputStream.close //yes... bad... but this is an iterator...
    }
  }

  def quickList[X](file: String)(implicit reader: CopyBookReader[X]) = foldLeft[List[X], X](file)(List[X]())((acc, x) => x :: acc)
  def list[X](file: String)(implicit reader: CopyBookReader[X]) = quickList[X](file).reverse
  def print[X](file: String)(implicit reader: CopyBookReader[X]) = foldLeft[Int, X](file)(0) { (acc, v) => println(v); acc + 1 }
  def justLoad[X](file: String)(implicit reader: CopyBookReader[X]) = foldLeft[Int, X](file)(0) { (acc, v) => acc + 1 }


}