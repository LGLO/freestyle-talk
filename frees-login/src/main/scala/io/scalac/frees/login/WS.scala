package io.scalac.frees.login

import cats.{Id, ~>}
import freestyle.free.free


case class WSResult(code: Int, headers: Map[String, Seq[String]], body: Array[Byte])

@free trait WS {
  def call(url: String, method: String): FS[WSResult]
}

object WSHandlers {
  implicit val wsIdHandler = new WS.Handler[Id] {
    override protected[this] def call(
      url: String,
      method: String
    ): Id[WSResult] = WSResult(200, Map.empty, Array.empty)
  }
}