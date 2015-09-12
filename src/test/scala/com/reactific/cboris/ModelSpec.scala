package com.reactific.cboris

import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant}

import Model._
import akka.util.ByteString
import org.specs2.mutable.Specification

import scala.collection.immutable.HashMap


object data {
  val bi = BigInt(0x11)
  val bd = BigDecimal(17.0)
  def unitFunc : Unit = {}
}

/** Test Cases For Model object */
class ModelSpec extends Specification {

  "Model" should {
    "Construct CBORValue from all the numeric types " in {
      val b : CBORValue = 0x11.toByte
      b.select[Byte] must beEqualTo(Some(0x11.toByte))
      val s : CBORValue = 0x11.toShort
      s.select[Short] must beEqualTo(Some(0x11.toShort))
      val i : CBORValue = 0x11
      i.select[Int] must beEqualTo(Some(0x11.toInt))
      val l : CBORValue = 0x11.toLong
      l.select[Long] must beEqualTo(Some(0x11.toLong))
      val bi : CBORValue = data.bi
      bi.select[BigInt] must beEqualTo(Some(data.bi))
      val f : CBORValue = 17.toFloat
      f.select[Float] must beEqualTo(Some(17.0))
      val d : CBORValue = 17D
      d.select[Double] must beEqualTo(Some(17.0D))
      val bd : CBORValue = data.bd
      bd.select[BigDecimal] must beEqualTo(Some(data.bd))
    }

    "Construct CBORvalue from constant values" in {
      val nul1 : CBORValue = CBORNull
      nul1.select[CBORNull.type] must beEqualTo(Some(CBORNull))
      val nul2 : CBORValue = data.unitFunc
      nul2.select[CBORNull.type] must beEqualTo(Some(CBORNull))
      val undef : CBORValue = CBORUndefined
      undef.select[CBORUndefined.type] must beEqualTo(Some(CBORUndefined))
      val b1 : CBORValue = true
      b1.select[Boolean] must beEqualTo(Some(true))
      val b2 : CBORValue = false
      b2.select[Boolean] must beEqualTo(Some(false))
    }

    "Construct CBORValue from time values" in {
      val t1 = Instant.now()
      val t2 = Instant.ofEpochSecond(17)
      val dur = Duration.between(t2, t1)
      val i1 : CBORValue = t1
      i1.select[Instant] must beEqualTo(Some(t1))
      val i2 : CBORValue = t2
      i2.select[Instant] must beEqualTo(Some(t2))
      val d : CBORValue = dur
      d.select[Duration] must beEqualTo(Some(dur))
    }

    "Construct CBORValue from strings" in {
      val s : CBORValue = "foo"
      s.select[String] must beEqualTo(Some("foo"))
      val bs_value : ByteString = ByteString("foo")
      val bs : CBORValue = bs_value
      bs.select[ByteString] must beEqualTo(Some(bs_value))
    }

    "Construct CBORValue from arrays" in {
      val array = CBORArray(Vector(17, 17.0D, "17", Instant.now()))
      val a : CBORValue = array
      a.select[CBORArray] must beEqualTo(Some(array))
    }

    "Construct CBORValue from maps" in {
      val aMap = new CBORMap( "a" -> 17, 23 -> "b", 17.0 -> Instant.now())
      val map : CBORValue = aMap
      map.select[CBORMap] must beEqualTo(Some(aMap))

    }
  }
}
