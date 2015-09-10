package com.reactific.cboris.codec

import akka.actor.ActorSystem
import akka.pattern.pipe
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.testkit.scaladsl._
import akka.testkit.TestProbe
import com.reactific.cboris.{CBORValue, CBORUndefined}
import org.specs2.mutable.Specification

import akka.util.{ByteStringBuilder, ByteString}
import scala.concurrent.Future
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

  implicit val system = ActorSystem("cboris-test")
  implicit val materializer = ActorMaterializer()

}

class DecoderSpec extends Specification {

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
    "reject garbage" in {
      val bs = utils.randomByteString(100)
      val src = Decoder(bs)
      val probe = TestProbe()
      val future : Future[CBORValue] = src.runWith(Sink.head)
      val expected = CBORUndefined()
      future.pipeTo(probe.ref)
      probe.expectMsg(100.millis,expected) must beEqualTo(expected)
    }
    "have some test examples" in {
      pending
    }
  }
}
