package com.reactific.cboris.codec

import akka.actor.ActorSystem
import akka.pattern.pipe
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.testkit.scaladsl._
import akka.testkit.TestProbe
import com.reactific.cboris.Model._
import com.reactific.cboris.Decode
import org.specs2.mutable.Specification

import akka.util.{ByteStringBuilder, ByteString}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

object utils {
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

}

class DecoderSpec extends Specification {

  sequential

  import utils._
  import system.dispatcher

  "randomByteString" should {
    "not return the same results" in {
      val bs1 = randomByteString(10)
      val bs2 = randomByteString(10)
      bs1.equals(bs2) must beFalse
    }
  }

  "Decoder" should {
    "reject unassigned codes" in {
      decode(0x1c) must throwAn[IllegalStateException]("Code 0x1c")
      decode(0x1d) must throwAn[IllegalStateException]("Code 0x1d")
      decode(0x1e) must throwAn[IllegalStateException]("Code 0x1e")
      decode(0x1f) must throwAn[IllegalStateException]("Code 0x1f")
      decode(0x3c) must throwAn[IllegalStateException]("Code 0x3c")
      decode(0x3d) must throwAn[IllegalStateException]("Code 0x3d")
      decode(0x3e) must throwAn[IllegalStateException]("Code 0x3e")
      decode(0x3f) must throwAn[IllegalStateException]("Code 0x3f")
      decode(0x5c) must throwAn[IllegalStateException]("Code 0x5c")
      decode(0x5d) must throwAn[IllegalStateException]("Code 0x5d")
      decode(0x5e) must throwAn[IllegalStateException]("Code 0x5e")
      decode(0x7c) must throwAn[IllegalStateException]("Code 0x7c")
      decode(0x7d) must throwAn[IllegalStateException]("Code 0x7d")
      decode(0x7e) must throwAn[IllegalStateException]("Code 0x7e")
      decode(0x9c) must throwAn[IllegalStateException]("Code 0x9c")
      decode(0x9d) must throwAn[IllegalStateException]("Code 0x9d")
      decode(0x9e) must throwAn[IllegalStateException]("Code 0x9e")
      decode(0xFF) must throwAn[IllegalStateException]("Unsupported initial byte: -1")
    }
    "reject garbage" in {
      val bs = utils.randomByteString(100)
      val probe = TestProbe()
      val src = Decode(bs)
      val future = src.runWith(Sink.head)
      Await.result(future, 1.second) must throwAn[IllegalStateException]("Unsupported")
    }
    "accept integers 0 to 23" in {
      decode(0x00) must beEqualTo(0)
      decode(0x01) must beEqualTo(1)
      decode(0x02) must beEqualTo(2)
      decode(0x03) must beEqualTo(3)
      decode(0x04) must beEqualTo(4)
      decode(0x05) must beEqualTo(5)
      decode(0x06) must beEqualTo(6)
      decode(0x07) must beEqualTo(7)
      decode(0x08) must beEqualTo(8)
      decode(0x09) must beEqualTo(9)
      decode(0x0a) must beEqualTo(10)
      decode(0x0b) must beEqualTo(11)
      decode(0x0c) must beEqualTo(12)
      decode(0x0d) must beEqualTo(13)
      decode(0x0e) must beEqualTo(14)
      decode(0x0f) must beEqualTo(15)
      decode(0x10) must beEqualTo(16)
      decode(0x11) must beEqualTo(17)
      decode(0x12) must beEqualTo(18)
      decode(0x13) must beEqualTo(19)
      decode(0x14) must beEqualTo(20)
      decode(0x15) must beEqualTo(21)
      decode(0x16) must beEqualTo(22)
      decode(0x17) must beEqualTo(23)
    }
    "accept negative integers -1 to -24" in {
      decode(0x20) must beEqualTo(-1)
      decode(0x21) must beEqualTo(-2)
      decode(0x22) must beEqualTo(-3)
      decode(0x23) must beEqualTo(-4)
      decode(0x24) must beEqualTo(-5)
      decode(0x25) must beEqualTo(-6)
      decode(0x26) must beEqualTo(-7)
      decode(0x27) must beEqualTo(-8)
      decode(0x28) must beEqualTo(-9)
      decode(0x29) must beEqualTo(-10)
      decode(0x2a) must beEqualTo(-11)
      decode(0x2b) must beEqualTo(-12)
      decode(0x2c) must beEqualTo(-13)
      decode(0x2d) must beEqualTo(-14)
      decode(0x2e) must beEqualTo(-15)
      decode(0x2f) must beEqualTo(-16)
      decode(0x30) must beEqualTo(-17)
      decode(0x31) must beEqualTo(-18)
      decode(0x32) must beEqualTo(-19)
      decode(0x33) must beEqualTo(-20)
      decode(0x34) must beEqualTo(-21)
      decode(0x35) must beEqualTo(-22)
      decode(0x36) must beEqualTo(-23)
      decode(0x37) must beEqualTo(-24)
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
  }
}
