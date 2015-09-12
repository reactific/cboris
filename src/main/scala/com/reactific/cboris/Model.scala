package com.reactific.cboris

import akka.util.ByteString

import java.time.Instant
import java.time.Duration

import shapeless._
import shapeless.ops.coproduct.Unifier

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object Model {

  sealed trait CBORVal

  case object CBORNull extends CBORVal

  case object CBORUndefined extends CBORVal

  final case class CBORArray(value: Vector[CBORValue]) extends CBORVal

  final case class CBORMap(
    value: HashMap[CBORValue, CBORValue] = HashMap.empty[CBORValue, CBORValue]
  ) extends CBORVal {
    def this( pairs: (Any,Any)* ) = this(HashMap( pairs.map {
      case (x,y) => atomFromAny(x) -> atomFromAny(y) }:_*))
  }

  /** CBOR Value Type
    * This Shapeless type is a union of the allowed CBOR types and is used as the Scala model for CBOR data values.
    */
  type CBORValue = Boolean :+: Byte :+: Short :+: Int :+: Long :+: Float :+: Double :+:
    BigInt :+: BigDecimal :+: ByteString :+: String :+: Instant :+: Duration :+:
    CBORNull.type :+: CBORUndefined.type :+: CBORArray :+: CBORMap :+: CNil

  implicit def atomFromUnit(u : Unit) : CBORValue = Coproduct[CBORValue](CBORNull)

  implicit def atomFromNull(n : CBORNull.type): CBORValue = Coproduct[CBORValue](n)

  implicit def atomFromUndefined(u: CBORUndefined.type) : CBORValue = Coproduct[CBORValue](u)

  implicit def atomFromBoolean(b: Boolean): CBORValue = Coproduct[CBORValue](b)

  implicit def atomFromByte(b: Byte): CBORValue = Coproduct[CBORValue](b)

  implicit def atomFromShort(s: Short): CBORValue = Coproduct[CBORValue](s)

  implicit def atomFromInt(i: Int): CBORValue = Coproduct[CBORValue](i)

  implicit def atomFromLong(l: Long): CBORValue = Coproduct[CBORValue](l)

  implicit def atomFromFloat(f: Float): CBORValue = Coproduct[CBORValue](f)

  implicit def atomFromDouble(d: Double): CBORValue = Coproduct[CBORValue](d)

  implicit def atomFromBigInt(bi: BigInt): CBORValue = Coproduct[CBORValue](bi)

  implicit def atomFromBigDecimal(bd: BigDecimal): CBORValue = Coproduct[CBORValue](bd)

  implicit def atomFromString(str: String): CBORValue = Coproduct[CBORValue](str)

  implicit def atomFromByteString(bs: ByteString): CBORValue = Coproduct[CBORValue](bs)

  implicit def atomFromInstant(inst: Instant): CBORValue = Coproduct[CBORValue](inst)

  implicit def atomFromDuration(dur: Duration): CBORValue = Coproduct[CBORValue](dur)

  implicit def atomFromArray(arr: CBORArray): CBORValue = Coproduct[CBORValue](arr)

  implicit def atomFromMap(map: CBORMap): CBORValue = Coproduct[CBORValue](map)

  implicit def atomFromAny(value : Any) : CBORValue = {
    value match {
      case b0: Boolean => atomFromBoolean(b0)
      case by: Byte => atomFromByte(by)
      case sh: Short => atomFromShort(sh)
      case i: Int => atomFromInt(i)
      case l: Long => atomFromLong(l)
      case f: Float => atomFromFloat(f)
      case d: Double => atomFromDouble(d)
      case inst : Instant => atomFromInstant(inst)
      case dur: Duration => atomFromDuration(dur)
      case bi: BigInt => atomFromBigInt(bi)
      case bd: BigDecimal => atomFromBigDecimal(bd)
      case nul: CBORNull.type => atomFromNull(CBORNull)
      case undef: CBORUndefined.type => atomFromUndefined(CBORUndefined)
      case str: String => atomFromString(str)
      case bs: ByteString => atomFromByteString(bs)
      case arr: CBORArray => atomFromArray(arr)
      case map: CBORMap => atomFromMap(map)
    }
  }

  /*
  object extractToAny extends Poly1 {
    implicit def caseBoolean : Any = at[Boolean] { b: Boolean ⇒ b }
    implicit def caseByte : Any = at[Byte] { b: Byte ⇒ b }
    implicit def caseShort : Any = at[Short] { s: Short ⇒ s }
    implicit def caseInt : Any = at[Int] { i: Int ⇒ i }
    implicit def caseLong : Any = at[Long] { l: Long ⇒ l }
    implicit def caseFloat : Any = at[Float] { f: Float ⇒ f }
    implicit def caseDouble : Any = at[Double] { d: Double ⇒ d }
    implicit def caseBigInt : Any = at[BigInt] { bi: BigInt ⇒  bi }
    implicit def caseBigDecimal : Any = at[BigDecimal] { bd : BigDecimal ⇒ bd }
    implicit def caseByteString : Any = at[ByteString] { bs : ByteString ⇒ bs }
    implicit def caseString : Any = at[String] { s : String ⇒ s }
    implicit def caseInstant : Any = at[Instant] { i : Instant ⇒ i }
    implicit def caseDuration : Any = at[Duration] { d : Duration ⇒ d  }
    implicit def caseNull : Any = at[CBORNull.type] { n : CBORNull.type ⇒ n}
    implicit def caseUndefined : Any = at[CBORUndefined.type] { n : CBORUndefined.type ⇒ n}
    implicit def caseArray : Any = at[CBORArray] { a : CBORArray ⇒ a }
    implicit def caseMap : Any = at[CBORMap] { a : CBORMap ⇒ a }
  }
  */

  def project[T](value: CBORValue)(implicit unify: Unifier.Aux[CBORValue, T]): T = {
    unify(value)
  }

  implicit def asAny( value : CBORValue) : Any = {
    project[Any](value)
  }


}
