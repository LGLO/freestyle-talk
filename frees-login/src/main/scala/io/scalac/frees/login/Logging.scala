package io.scalac.frees.login

import cats.Id
import freestyle.free._
import freestyle.free.implicits._

/**
  * Just `info` and `warn` for brevity. There is LoggingM build in Freestyle also.
  */
@free trait Logging {
  def info(msg: String): FS[Unit]
  def warn(msg: String): FS[Unit]
  def error(msg: String, thr: Option[Throwable]): FS[Unit]
}

object LoggingHandlers {
  implicit val id = new Logging.Handler[Id] {
    override def info(msg: String) = println(s"[INFO] $msg")

    override def warn(msg: String) = println(s"[WARN] $msg")

    override def error(msg: String, thr: Option[Throwable]) = {
      println(s"[ERROR]: $msg, $thr")
      thr.foreach(_.printStackTrace)
    }
  }
}
