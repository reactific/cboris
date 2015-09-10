package com.reactific.cboris.codec

import akka.stream.scaladsl.{Source, Flow}
import akka.util.ByteString
import com.reactific.cboris.{CBORUndefined, CBORValue, CBORDocument}

object Decoder {
  type CBORiSFlow = Flow[ByteString, CBORValue, Unit]

  def apply(bs : ByteString) : Source[CBORValue, Unit] = {
    flow()
    Source.single(bs).via(flow())
  }

  def flow() : CBORiSFlow = {
    Flow[ByteString].map[CBORValue] { bs => decode(bs)  }
  }

  def decode(bs: ByteString) : CBORValue = {
    CBORUndefined()
  }
}

trait Decoder extends ( (ByteString) => CBORDocument ) {

}

