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

  def decode1(bs: ByteString) : Any = {
    implicit val execution_context = system.dispatcher
    val src = Decode(bs)
    val future : Future[Any] = src.runWith(Sink.head).map { value => asAny(value) }
    Await.result(future, 1.second)
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
      decode1(byteString(0x1c)) must throwAn[IllegalStateException]("Code 0x1c")
      decode1(byteString(0x1d)) must throwAn[IllegalStateException]("Code 0x1d")
      decode1(byteString(0x1e)) must throwAn[IllegalStateException]("Code 0x1e")
      decode1(byteString(0x1f)) must throwAn[IllegalStateException]("Code 0x1f")
      decode1(byteString(0x3c)) must throwAn[IllegalStateException]("Code 0x3c")
      decode1(byteString(0x3d)) must throwAn[IllegalStateException]("Code 0x3d")
      decode1(byteString(0x3e)) must throwAn[IllegalStateException]("Code 0x3e")
      decode1(byteString(0x3f)) must throwAn[IllegalStateException]("Code 0x3f")
      decode1(byteString(0x5c)) must throwAn[IllegalStateException]("Code 0x5c")
      decode1(byteString(0x5d)) must throwAn[IllegalStateException]("Code 0x5d")
      decode1(byteString(0x5e)) must throwAn[IllegalStateException]("Code 0x5e")
      decode1(byteString(0x7c)) must throwAn[IllegalStateException]("Code 0x7c")
      decode1(byteString(0x7d)) must throwAn[IllegalStateException]("Code 0x7d")
      decode1(byteString(0x7e)) must throwAn[IllegalStateException]("Code 0x7e")
      decode1(byteString(-100)) must throwAn[IllegalStateException]("Code 0x9c")
      decode1(byteString(-99)) must throwAn[IllegalStateException]("Code 0x9d")
      decode1(byteString(-98)) must throwAn[IllegalStateException]("Code 0x9e")
      decode1(byteString(0xFF.toByte)) must throwAn[IllegalStateException]("Unsupported initial byte: -1")
    }
    "reject garbage" in {
      val bs = utils.randomByteString(100)
      val probe = TestProbe()
      val src = Decode(bs)
      val future = src.runWith(Sink.head)
      Await.result(future, 1.second) must throwAn[IllegalStateException]("Unsupported")
    }
    "accept integers 0-23" in {
      decode1(byteString(0)) must beEqualTo(0)
      decode1(byteString(1)) must beEqualTo(1)
      decode1(byteString(2)) must beEqualTo(2)
      decode1(byteString(3)) must beEqualTo(3)
      decode1(byteString(4)) must beEqualTo(4)
      decode1(byteString(5)) must beEqualTo(5)
      decode1(byteString(6)) must beEqualTo(6)
      decode1(byteString(7)) must beEqualTo(7)
      decode1(byteString(8)) must beEqualTo(8)
      decode1(byteString(9)) must beEqualTo(9)
      decode1(byteString(10)) must beEqualTo(10)
      decode1(byteString(11)) must beEqualTo(11)
      decode1(byteString(12)) must beEqualTo(12)
      decode1(byteString(13)) must beEqualTo(13)
      decode1(byteString(14)) must beEqualTo(14)
      decode1(byteString(15)) must beEqualTo(15)
      decode1(byteString(16)) must beEqualTo(16)
      decode1(byteString(17)) must beEqualTo(17)
      decode1(byteString(18)) must beEqualTo(18)
      decode1(byteString(19)) must beEqualTo(19)
      decode1(byteString(20)) must beEqualTo(20)
      decode1(byteString(21)) must beEqualTo(21)
      decode1(byteString(22)) must beEqualTo(22)
      decode1(byteString(23)) must beEqualTo(23)
    }
  }
}
