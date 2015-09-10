package com.reactific.cboris

import scala.collection.immutable.HashMap

sealed trait CBORValue {
  def value : Any
}

final case class CBORUndefined(value : Unit = Unit) extends CBORValue

case class CBORString(value : String = "") extends CBORValue

case class CBORDocument(
  value : HashMap[String,CBORValue] = HashMap. empty[String,CBORValue]
) extends CBORValue
