package com.reactific.cboris

import java.nio.charset.StandardCharsets

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.testkit.TestProbe
import akka.util.{ByteString, ByteStringBuilder}
import com.reactific.cboris.Model._
import org.specs2.mutable.Specification

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object utils {
  def randomString(len : Int) : String = {
    val array = new Array[Char](len)
    for (x <- 1 to len) {
      array.update(x-1, Random.nextPrintableChar())
    }
    new String(array)
  }

  def randomByteString(len : Int, assureInvalid: Boolean = true) : ByteString = {
    //create bytestring containing random bytes
    val randomBytes = new Array[Byte](len - (if (assureInvalid) 3 else 0))
    Random.nextBytes(randomBytes)
    val bldr = new ByteStringBuilder
    if (assureInvalid) {
      bldr.putByte(0xFC.toByte)
      bldr.putByte(0xFD.toByte)
      bldr.putByte(0xFE.toByte)
    }
    bldr.putBytes(randomBytes)
    bldr.result()
  }

  def byteString(byte : Byte) : ByteString = {
    val bldr = new ByteStringBuilder
    bldr.putByte(byte.toByte)
    bldr.result()
  }

  implicit val system = ActorSystem("cboris-test")
  implicit val materializer = ActorMaterializer()

  def decodebs(bs: ByteString) : Any = {
    implicit val execution_context = system.dispatcher
    val src = Decode(bs)
    val future : Future[Any] = src.runWith(Sink.head).map { value => asAny(value) }
    Await.result(future, 1.second)
  }

  def decode(bytes: Int*) : Any = {
    implicit val execution_context = system.dispatcher
    val bldr = new ByteStringBuilder
    for (byte <- bytes) { bldr.putByte(byte.toByte) }
    decodebs(bldr.result())
  }

  def decode(bs: ByteString, bytes: Int*) : Any = {
    val bldr = new ByteStringBuilder
    for (b <- bytes) { bldr.putByte(b.toByte) }
    bldr.append(bs)
    decodebs(bldr.result())
  }

}

class DecoderSpec extends Specification {

  sequential

  import utils._

  "randomByteString" should {
    "not return the same results" in {
      val bs1 = randomByteString(10)
      val bs2 = randomByteString(10)
      bs1.equals(bs2) must beFalse
    }
  }

  "Decoder" should {
    "reject unassigned codes" in {
      decode(0x1c) must throwAn[IllegalStateException]("Unsupported: Code 0x1c")
      decode(0x1d) must throwAn[IllegalStateException]("Unsupported: Code 0x1d")
      decode(0x1e) must throwAn[IllegalStateException]("Unsupported: Code 0x1e")
      decode(0x1f) must throwAn[IllegalStateException]("Unsupported: Code 0x1f")
      decode(0x3c) must throwAn[IllegalStateException]("Unsupported: Code 0x3c")
      decode(0x3d) must throwAn[IllegalStateException]("Unsupported: Code 0x3d")
      decode(0x3e) must throwAn[IllegalStateException]("Unsupported: Code 0x3e")
      decode(0x3f) must throwAn[IllegalStateException]("Unsupported: Code 0x3f")
      decode(0x5c) must throwAn[IllegalStateException]("Unsupported: Code 0x5c")
      decode(0x5d) must throwAn[IllegalStateException]("Unsupported: Code 0x5d")
      decode(0x5e) must throwAn[IllegalStateException]("Unsupported: Code 0x5e")
      decode(0x7c) must throwAn[IllegalStateException]("Unsupported: Code 0x7c")
      decode(0x7d) must throwAn[IllegalStateException]("Unsupported: Code 0x7d")
      decode(0x7e) must throwAn[IllegalStateException]("Unsupported: Code 0x7e")
      decode(0x9c) must throwAn[IllegalStateException]("Unsupported: Code 0x9c")
      decode(0x9d) must throwAn[IllegalStateException]("Unsupported: Code 0x9d")
      decode(0x9e) must throwAn[IllegalStateException]("Unsupported: Code 0x9e")
      decode(0xbc) must throwAn[IllegalStateException]("Unsupported: Code 0xbc")
      decode(0xbd) must throwAn[IllegalStateException]("Unsupported: Code 0xbd")
      decode(0xbe) must throwAn[IllegalStateException]("Unsupported: Code 0xbe")
      decode(0xdc) must throwAn[IllegalStateException]("Unsupported: Code 0xdc")
      decode(0xdd) must throwAn[IllegalStateException]("Unsupported: Code 0xdd")
      decode(0xde) must throwAn[IllegalStateException]("Unsupported: Code 0xde")
      decode(0xfc) must throwAn[IllegalStateException]("Unsupported: Code 0xfc")
      decode(0xfd) must throwAn[IllegalStateException]("Unsupported: Code 0xfd")
      decode(0xfe) must throwAn[IllegalStateException]("Unsupported: Code 0xfe")
    }
    "reject garbage" in {
      val bs = utils.randomByteString(100)
      val probe = TestProbe()
      val src = Decode(bs)
      val future = src.runWith(Sink.head)
      Await.result(future, 1.second) must throwAn[IllegalStateException]("Unsupported")
    }
    "accept integers 0 to 23" in {
      for (x <- 0x00 to 0x17) {
        decode(x) must beEqualTo(x)
      }
      success
    }
    "accept negative integers -1 to -24" in {
      for (x <- 0x20 to 0x37) {
        decode(x) must beEqualTo( -(x - 0x1f))
      }
      success
    }
    "accept small unsigned integers of 1, 2, 4, and 8 bytes" in {
      decode(0x18,0x11) must beEqualTo(0x11.toByte)
      decode(0x19,0x00,0x11) must beEqualTo(0x11.toShort)
      decode(0x1a,0x00,0x00,0x00,0x11) must beEqualTo(0x11)
      decode(0x1b,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x11) must beEqualTo(0x11.toLong)
    }
    "accept small negative integers of 1, 2, 4, and 8 bytes" in {
      decode(0x38,0x11) must beEqualTo(-18.toShort)
      decode(0x39,0x00,0x11) must beEqualTo(-18.toInt)
      decode(0x3a,0x00,0x00,0x00,0x11) must beEqualTo(-18.toLong)
      decode(0x3b,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x11) must beEqualTo(BigInt(-18))
    }
    "accept big unsigned integers of 1, 2, 4, and 8 bytes" in {
      decode(0x18,0xFF) must beEqualTo(Byte.MaxValue.toShort*2+1)
      decode(0x19,0xFF,0xFF) must beEqualTo(Short.MaxValue.toInt*2+1)
      decode(0x1a,0xFF,0xFF,0xFF,0xFF) must beEqualTo(Int.MaxValue.toLong*2+1)
      decode(0x1b,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF) must beEqualTo(BigInt(Long.MaxValue)*2+1)
    }
    "accept big negative integers of 1, 2, 4, and 8 bytes" in {
      decode(0x38,0xFF) must beEqualTo(Byte.MinValue.toShort*2)
      decode(0x39,0xFF,0xFF) must beEqualTo(Short.MinValue.toInt*2)
      decode(0x3a,0xFF,0xFF,0xFF,0xFF) must beEqualTo(Int.MinValue.toLong*2)
      decode(0x3b,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF) must beEqualTo(BigInt(Long.MinValue)*2)
    }
    "accept definite byte strings" in {
      decode(0x40) must beEqualTo(ByteString())
      for (x <- 0x41 to 0x57) {
        val bs = randomByteString(x - 0x40, assureInvalid = false)
        decode(bs, x) must beEqualTo(bs)
      }
      val bs = randomByteString(0x17, assureInvalid = false)
      decode(bs, 0x58, 0x17) must beEqualTo(bs)
      decode(bs, 0x59, 0x00, 0x17) must beEqualTo(bs)
      decode(bs, 0x5a, 0x00, 0x00, 0x00, 0x17) must beEqualTo(bs)
      decode(bs, 0x5b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17) must beEqualTo(bs)
    }
    "accept indefinite byte strings" in {
      val numStrs = Random.nextInt(10) + 1
      val concat = ByteString.newBuilder
      val bldr = ByteString.newBuilder
      bldr.putByte(0x5f)
      for (x <- 1 to numStrs) {
        val bsLen = Random.nextInt(10)
        bldr.putByte(0x59)
        bldr.putByte(0x00).putByte(bsLen.toByte)
        val bs = randomByteString(bsLen, assureInvalid = false)
        bldr.append(bs)
        concat.append(bs)
      }
      bldr.putByte(0xff.toByte) // termination signal
      val bs = bldr.result()
      val result = decode(bs)
      result must beEqualTo(concat.result())
    }
    "accept definite text strings" in {
      decode(0x60) must beEqualTo(new String)
      for (x <- 0x60 to 0x77) {
        val str = randomString(x - 0x60)
        val bs = ByteString(str.getBytes(StandardCharsets.UTF_8))
        decode(bs, x) must beEqualTo(str)
      }
      val str = randomString(Random.nextInt(100))
      val bs = ByteString(str.getBytes)
      val len = bs.length
      decode(bs, 0x78, len) must beEqualTo(str)
      decode(bs, 0x79, 0x00, len) must beEqualTo(str)
      decode(bs, 0x7a, 0x00, 0x00, 0x00, len) must beEqualTo(str)
      decode(bs, 0x7b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, len) must beEqualTo(str)
    }
    "accept indefinite text strings" in {
      val numStrs = Random.nextInt(10) + 1
      val concat = new StringBuilder
      val bldr = ByteString.newBuilder
      bldr.putByte(0x7f)
      for (x <- 1 to numStrs) {
        val bsLen = Random.nextInt(10)
        val str = randomString(bsLen)
        val bs = ByteString(str.getBytes)
        bldr.putByte(0x79)
        bldr.putByte(0x00).putByte(bs.length.toByte)
        bldr.append(bs)
        concat.append(str)
      }
      bldr.putByte(0xff.toByte) // termination signal
      val bs = bldr.result()
      val result = decode(bs)
      result must beEqualTo(concat.result())
    }

    "accept constant values" in {
      decode(0xf4) must beEqualTo(false)
      decode(0xf5) must beEqualTo(true)
      decode(0xf6) must beEqualTo(CBORNull)
      decode(0xf7) must beEqualTo(CBORUndefined)
    }
  }
}
