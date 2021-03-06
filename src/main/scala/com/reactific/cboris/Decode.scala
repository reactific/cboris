package com.reactific.cboris

import java.nio.charset.StandardCharsets

import akka.stream.scaladsl.{Flow, Source}
import akka.util.{ByteIterator, ByteString}
import com.reactific.cboris.Model._

import scala.annotation.switch

object Decode {
  type CBORiSFlow = Flow[ByteString, CBORValue, Unit]

  def apply(ba : Array[Byte]) : Source[CBORValue, Unit] = {
    Source.single(ByteString(ba)).via(flow())
  }

  def apply(bs : ByteString) : Source[CBORValue, Unit] = {
    Source.single(bs).via(flow())
  }

  def flow() : CBORiSFlow = {
    Flow[ByteString].map[CBORValue] { bs => decode(bs)  }
  }

  private def unsupported(s : String) : Unit = { throw new IllegalStateException(s) }

  private def unsignedByte(byte : Byte) : Short = {
    (byte & 0xff).toShort
  }

  private def read_uint8(bi: ByteIterator) : CBORValue = {
    val byte : Byte = bi.next()
    val result : CBORValue = if (byte >= 0) {
      byte
    } else {
      unsignedByte(byte)
    }
    result
  }

  private def get_uint8(bi: ByteIterator) : Short = {
    val value = read_uint8(bi)
    value.select[Byte] match {
      case Some(x) => x.toShort
      case None => value.select[Short].get
    }
  }

  private def read_uint16(bi: ByteIterator) : CBORValue = {
    val b1 : Short = unsignedByte(bi.next())
    val b2 : Short = unsignedByte(bi.next())
    val result = if (b1 < 128) {
      (b1 << 8) + b2
    } else {
      (b1.toInt << 8) + b2.toInt
    }
    result
  }

  private def get_uint16(bi: ByteIterator) : Int = {
    val value = read_uint16(bi)
    value.select[Short] match {
      case Some(x) => x.toInt
      case None => value.select[Int].get
    }
  }

  private def read_uint32(bi : ByteIterator) : CBORValue = {
    val b1 : Short = unsignedByte(bi.next())
    val b2 : Short = unsignedByte(bi.next())
    val b3 : Short = unsignedByte(bi.next())
    val b4 : Short = unsignedByte(bi.next())
    val result = if (b1 < 128) {
      (b1.toInt << 24) + (b2.toInt << 16) + (b3.toInt << 8) + b4.toInt
    } else {
      (b1.toLong << 24) + (b2.toLong << 16) + (b3.toLong << 8) + b4.toLong
    }
    result
  }

  private def get_uint32(bi: ByteIterator) : Long = {
    val value = read_uint32(bi)
    value.select[Int] match {
      case Some(x) => x.toLong
      case None => value.select[Long].get
    }
  }


  private def read_uint64(bi: ByteIterator) : CBORValue = {
    val b1 : Short = unsignedByte(bi.next())
    val b2 : Short = unsignedByte(bi.next())
    val b3 : Short = unsignedByte(bi.next())
    val b4 : Short = unsignedByte(bi.next())
    val b5 : Short = unsignedByte(bi.next())
    val b6 : Short = unsignedByte(bi.next())
    val b7 : Short = unsignedByte(bi.next())
    val b8 : Short = unsignedByte(bi.next())
    val result : CBORValue = if (b1 < 128 ) {
      (b1.toLong << 56) + (b2.toLong << 48) + (b3.toLong << 40) + (b4.toLong << 32) +
        (b5.toLong << 24) + (b6.toLong << 16) + (b7.toLong << 8) + b8.toLong
    } else {
      (BigInt(b1) << 56) + (BigInt(b2) << 48) + (BigInt(b3) << 40) + (BigInt(b4) << 32) +
        (BigInt(b5) << 24) + (BigInt(b6) << 16) + (BigInt(b7) << 8) + BigInt(b8)
    }
    result
  }

  private def get_uint64(bi: ByteIterator) : BigInt = {
    val value = read_uint64(bi)
    value.select[Long] match {
      case Some(x) => BigInt(x)
      case None => value.select[BigInt].get
    }
  }

  private def read_array(bi: ByteIterator, size : Int) : CBORArray = {
    val result = Vector.newBuilder[CBORValue]
    result.sizeHint(size)
    for (index : Int <- 0 to size) { result += decode(bi) }
    CBORArray(result.result())
  }

  private def read_bytes(bi: ByteIterator, count: Int) : ByteIterator = {
    val clone = bi.clone()
    bi.drop(count)
    clone.take(count)
  }

  private def read_indefinite_array(bi : ByteIterator) : CBORArray = {
    val result = Vector.newBuilder[CBORValue]
    result.sizeHint(32)
    while (bi.hasNext && bi.head != 0xff) {
      result += decode(bi)
    }
    CBORArray(result.result())
  }


  def decode_positive_integer(bi : ByteIterator) : CBORValue = {
    val byte = bi.next()
    val byteUnsigned = byte & 0xff
    (byteUnsigned: @switch) match {
      case 0x00 => 0
      case 0x01 => 1
      case 0x02 => 2
      case 0x03 => 3
      case 0x04 => 4
      case 0x05 => 5
      case 0x06 => 6
      case 0x07 => 7
      case 0x08 => 8
      case 0x09 => 9
      case 0x0a => 10
      case 0x0b => 11
      case 0x0c => 12
      case 0x0d => 13
      case 0x0e => 14
      case 0x0f => 15
      case 0x10 => 16
      case 0x11 => 17
      case 0x12 => 18
      case 0x13 => 19
      case 0x14 => 20
      case 0x15 => 21
      case 0x16 => 22
      case 0x17 => 23
      case 0x18 => read_uint8(bi)
      case 0x19 => read_uint16(bi)
      case 0x1a => read_uint32(bi)
      case 0x1b => read_uint64(bi)
      case 0x1c => unsupported("Unsupported: Code 0x1c")
      case 0x1d => unsupported("Unsupported: Code 0x1d")
      case 0x1e => unsupported("Unsupported: Code 0x1e")
      case 0x1f => unsupported("Unsupported: Code 0x1f")
      case _ => unsupported(f"Invalid initial byte $byteUnsigned%x for positive integer")
    }
  }

  def decode_negative_integer(bi : ByteIterator) : CBORValue = {
    val byte = bi.next()
    val byteUnsigned = byte & 0xff
    (byteUnsigned: @switch) match {
      case 0x20 => -1
      case 0x21 => -2
      case 0x22 => -3
      case 0x23 => -4
      case 0x24 => -5
      case 0x25 => -6
      case 0x26 => -7
      case 0x27 => -8
      case 0x28 => -9
      case 0x29 => -10
      case 0x2a => -11
      case 0x2b => -12
      case 0x2c => -13
      case 0x2d => -14
      case 0x2e => -15
      case 0x2f => -16
      case 0x30 => -17
      case 0x31 => -18
      case 0x32 => -19
      case 0x33 => -20
      case 0x34 => -21
      case 0x35 => -22
      case 0x36 => -23
      case 0x37 => -24
      case 0x38 => -1.toShort - get_uint8(bi)
      case 0x39 => -1 - get_uint16(bi)
      case 0x3a => -1L - get_uint32(bi)
      case 0x3b => BigInt(-1) - get_uint64(bi)
      case 0x3c => unsupported("Unsupported: Code 0x3c")
      case 0x3d => unsupported("Unsupported: Code 0x3d")
      case 0x3e => unsupported("Unsupported: Code 0x3e")
      case 0x3f => unsupported("Unsupported: Code 0x3f")
    }
  }

  def decode_bytestring(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    if (byte == 0x5f)
      decode_indefinite_bytestring(bi)
    else
      decode_definite_bytestring(byte, bi)
  }

  private def decode_definite_bytestring(byte: Int, bi : ByteIterator): ByteString = {
    (byte: @switch) match {
      case 0x40 => read_bytes(bi,0).toByteString
      case 0x41 => read_bytes(bi,1).toByteString
      case 0x42 => read_bytes(bi,2).toByteString
      case 0x43 => read_bytes(bi,3).toByteString
      case 0x44 => read_bytes(bi,4).toByteString
      case 0x45 => read_bytes(bi,5).toByteString
      case 0x46 => read_bytes(bi,6).toByteString
      case 0x47 => read_bytes(bi,7).toByteString
      case 0x48 => read_bytes(bi,8).toByteString
      case 0x49 => read_bytes(bi,9).toByteString
      case 0x4a => read_bytes(bi,10).toByteString
      case 0x4b => read_bytes(bi,11).toByteString
      case 0x4c => read_bytes(bi,12).toByteString
      case 0x4d => read_bytes(bi,13).toByteString
      case 0x4e => read_bytes(bi,14).toByteString
      case 0x4f => read_bytes(bi,15).toByteString
      case 0x50 => read_bytes(bi,16).toByteString
      case 0x51 => read_bytes(bi,17).toByteString
      case 0x52 => read_bytes(bi,18).toByteString
      case 0x53 => read_bytes(bi,19).toByteString
      case 0x54 => read_bytes(bi,20).toByteString
      case 0x55 => read_bytes(bi,21).toByteString
      case 0x56 => read_bytes(bi,22).toByteString
      case 0x57 => read_bytes(bi,23).toByteString
      case 0x58 => read_bytes(bi, get_uint8(bi)).toByteString
      case 0x59 => read_bytes(bi, get_uint16(bi)).toByteString
      case 0x5a => read_bytes(bi, get_uint32(bi).toInt).toByteString
      case 0x5b => read_bytes(bi, get_uint64(bi).toInt).toByteString
      case 0x5c => unsupported("Unsupported: Code 0x5c") ; ByteString()
      case 0x5d => unsupported("Unsupported: Code 0x5d") ; ByteString()
      case 0x5e => unsupported("Unsupported: Code 0x5e") ; ByteString()
      case _ => unsupported(f"Unsupported: Code $byte%x is not a definite byte string") ; ByteString()
    }
  }

  def decode_indefinite_bytestring(bi : ByteIterator) : ByteString = {
    val result = ByteString.newBuilder
    while (bi.hasNext && (bi.head != 0xff.toByte)) {
      val bs = decode_definite_bytestring(bi.next(), bi)
      result.append(bs)
    }
    result.result()
  }

  def decode_textstring(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    if (byte == 0x7f)
      decode_indefinite_textstring(bi)
    else
      decode_definite_textstring(byte, bi)
  }

  def decode_definite_textstring(byte: Int, bi : ByteIterator) : String = {
    (byte: @switch) match {
      case 0x60 => new String
      case 0x61 => new String(read_bytes(bi,1).toArray, StandardCharsets.UTF_8)
      case 0x62 => new String(read_bytes(bi,2).toArray, StandardCharsets.UTF_8)
      case 0x63 => new String(read_bytes(bi,3).toArray, StandardCharsets.UTF_8)
      case 0x64 => new String(read_bytes(bi,4).toArray, StandardCharsets.UTF_8)
      case 0x65 => new String(read_bytes(bi,5).toArray, StandardCharsets.UTF_8)
      case 0x66 => new String(read_bytes(bi,6).toArray, StandardCharsets.UTF_8)
      case 0x67 => new String(read_bytes(bi,7).toArray, StandardCharsets.UTF_8)
      case 0x68 => new String(read_bytes(bi,8).toArray, StandardCharsets.UTF_8)
      case 0x69 => new String(read_bytes(bi,9).toArray, StandardCharsets.UTF_8)
      case 0x6a => new String(read_bytes(bi,10).toArray, StandardCharsets.UTF_8)
      case 0x6b => new String(read_bytes(bi,11).toArray, StandardCharsets.UTF_8)
      case 0x6c => new String(read_bytes(bi,12).toArray, StandardCharsets.UTF_8)
      case 0x6d => new String(read_bytes(bi,13).toArray, StandardCharsets.UTF_8)
      case 0x6e => new String(read_bytes(bi,14).toArray, StandardCharsets.UTF_8)
      case 0x6f => new String(read_bytes(bi,15).toArray, StandardCharsets.UTF_8)
      case 0x70 => new String(read_bytes(bi,16).toArray, StandardCharsets.UTF_8)
      case 0x71 => new String(read_bytes(bi,17).toArray, StandardCharsets.UTF_8)
      case 0x72 => new String(read_bytes(bi,18).toArray, StandardCharsets.UTF_8)
      case 0x73 => new String(read_bytes(bi,19).toArray, StandardCharsets.UTF_8)
      case 0x74 => new String(read_bytes(bi,20).toArray, StandardCharsets.UTF_8)
      case 0x75 => new String(read_bytes(bi,21).toArray, StandardCharsets.UTF_8)
      case 0x76 => new String(read_bytes(bi,22).toArray, StandardCharsets.UTF_8)
      case 0x77 => new String(read_bytes(bi,23).toArray, StandardCharsets.UTF_8)
      case 0x78 => new String(read_bytes(bi,get_uint8(bi).toInt).toArray, StandardCharsets.UTF_8)
      case 0x79 => new String(read_bytes(bi,get_uint16(bi)).toArray, StandardCharsets.UTF_8)
      case 0x7a => new String(read_bytes(bi,get_uint32(bi).toInt).toArray, StandardCharsets.UTF_8)
      case 0x7b => new String(read_bytes(bi,get_uint64(bi).toInt).toArray, StandardCharsets.UTF_8)
      case 0x7c => unsupported("Unsupported: Code 0x7c") ; new String
      case 0x7d => unsupported("Unsupported: Code 0x7d") ; new String
      case 0x7e => unsupported("Unsupported: Code 0x7e") ; new String
      case _ => unsupported(f"Unsupported: Code $byte%x is not a definite text string"); new String
    }
  }

  private def decode_indefinite_textstring(bi : ByteIterator) : String = {
    val result = new StringBuilder
    while (bi.hasNext & (bi.head != 0xff.toByte)) {
      val bs = decode_definite_textstring(bi.next(), bi)
      result.append(bs)
    }
    result.result()
  }

  def decode_array(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    (byte: @switch) match {
      case 0x80 => read_array(bi, 0)
      case 0x81 => read_array(bi, 1)
      case 0x82 => read_array(bi, 2)
      case 0x83 => read_array(bi, 3)
      case 0x84 => read_array(bi, 4)
      case 0x85 => read_array(bi, 5)
      case 0x86 => read_array(bi, 6)
      case 0x87 => read_array(bi, 7)
      case 0x88 => read_array(bi, 8)
      case 0x89 => read_array(bi, 9)
      case 0x8a => read_array(bi, 10)
      case 0x8b => read_array(bi, 11)
      case 0x8c => read_array(bi, 12)
      case 0x8d => read_array(bi, 13)
      case 0x8e => read_array(bi, 14)
      case 0x8f => read_array(bi, 15)
      case 0x90 => read_array(bi, 16)
      case 0x91 => read_array(bi, 17)
      case 0x92 => read_array(bi, 18)
      case 0x93 => read_array(bi, 19)
      case 0x94 => read_array(bi, 20)
      case 0x95 => read_array(bi, 21)
      case 0x96 => read_array(bi, 22)
      case 0x97 => read_array(bi, 23)
      case 0x98 => read_array(bi, get_uint8(bi).toInt)
      case 0x99 => read_array(bi, get_uint16(bi))
      case 0x9a => read_array(bi, get_uint32(bi).toInt)
      case 0x9b => read_array(bi, get_uint64(bi).toInt)
      case 0x9c => unsupported("Unsupported: Code 0x9c")
      case 0x9d => unsupported("Unsupported: Code 0x9d")
      case 0x9e => unsupported("Unsupported: Code 0x9e")
      case 0x9f => read_indefinite_array(bi)
      case _ => unsupported(f"Unsupported: Code $byte%x is not an array"); new String
    }
  }

  def decode_map(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    (byte: @switch) match {
      case 0xbc => unsupported("Unsupported: Code 0xbc")
      case 0xbd => unsupported("Unsupported: Code 0xbd")
      case 0xbe => unsupported("Unsupported: Code 0xbe")
      case _ => unsupported(f"Unsupported: Code $byte%x is not a map"); new String
    }
  }

  def decode_tagged_type(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    (byte: @switch) match {
      case 0xdc => unsupported("Unsupported: Code 0xdc")
      case 0xdd => unsupported("Unsupported: Code 0xdd")
      case 0xde => unsupported("Unsupported: Code 0xde")
      case _ => unsupported(f"Unsupported: Code $byte%x is not a tagged type"); new String
    }
  }

  def decode_fp_and_simple(bi : ByteIterator) : CBORValue = {
    val byte = bi.next() & 0xff
    (byte: @switch) match {
      case 0xf4 => false
      case 0xf5 => true
      case 0xf6 => CBORNull
      case 0xf7 => CBORUndefined
      case 0xfc => unsupported("Unsupported: Code 0xfc")
      case 0xfd => unsupported("Unsupported: Code 0xfd")
      case 0xfe => unsupported("Unsupported: Code 0xfe")
      case _ => unsupported( f"Unsupported: Code 0x$byte%2x")
    }
  }

  def decode(bi: ByteIterator) : CBORValue = {
    if (bi.hasNext) {
      ((bi.head & 0xff) >> 5: @switch) match {
        case 0 => decode_positive_integer(bi)
        case 1 => decode_negative_integer(bi)
        case 2 => decode_bytestring(bi)
        case 3 => decode_textstring(bi)
        case 4 => decode_array(bi)
        case 5 => decode_map(bi)
        case 6 => decode_tagged_type(bi)
        case 7 => decode_fp_and_simple(bi)
      }
    } else {
      CBORNull
    }
  }

  def decode(bs: ByteString) : CBORValue = {
    decode(bs.iterator)
  }

}
