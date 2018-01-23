package multiple

import cats.{Id, InjectK, ~>}
import cats.free._
import cats.data.EitherK
import multiple.MultipleAlgebras.Logging.{Debug, Error, Info}
import multiple.MultipleAlgebras.WS.Call
import single.ExampleAlgebra.UserDB.{Insert, QueryByEmail}
import single.ExampleAlgebra._
import single.UserInsertionResult
import single.UserInsertionResult._

import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultipleAlgebras {

  // ESSENTIAL: Logging algebra
  sealed trait Logging[A]

  object Logging {
    final case class Debug(msg: String) extends Logging[Unit]
    final case class Info(msg: String) extends Logging[Unit]
    final case class Error(msg: String, throwable: Option[Throwable]) extends Logging[Unit]
  }

  // ESSENTIAL: Web services algebra
  case class WSResult(code: Int, headers: Map[String, Seq[String]], body: Array[Byte])

  trait WS[A]

  object WS {
    final case class Call(url: String, method: String) extends WS[WSResult]
  }

  //BOILERPLATE
  type MyApp[A] = EitherK[UserDB, Logging, A]
  //even more fun with WS algebra! Imagine having 6 or so algebras!
  type AppWithWS[A] = EitherK[MyApp, WS, A]

  //BOILERPLATE - smart constructors. Free.inject is like Free.liftM but for EitherK.
  class UserDBOperations[F[_]](implicit I: InjectK[UserDB, F]) {
    def insert(e: Email, p: PasswordHash) = Free.inject[UserDB, F](Insert(e, p))
    def queryByEmail(e: Email) = Free.inject[UserDB, F](QueryByEmail(e))
  }

  //BOILERPLATE - InjectK
  object UserDBOperations {
    implicit def userDBOperations[F[_]](
      implicit I: InjectK[UserDB, F]
    ): UserDBOperations[F] = new UserDBOperations[F]
  }

  //BOILERPLATE - InjectK
  class LoggingOperations[F[_]](implicit I: InjectK[Logging, F]) {
    def debug(msg: String) = Free.inject[Logging, F](Debug(msg))
    def info(msg: String) = Free.inject[Logging, F](Info(msg))
    def error(msg: String, thr: Option[Throwable]) = Free.inject[Logging, F](Error(msg, thr))
  }

  //BOILERPLATE
  object LoggingOperations {

    implicit def loggingOperations[F[_]](
      implicit I: InjectK[Logging, F]
    ): LoggingOperations[F] = new LoggingOperations[F]
  }

  def saveUser(e: Email, p: PasswordHash)
    (implicit D: UserDBOperations[MyApp], L: LoggingOperations[MyApp]) //BOILERPLATE line
  : Free[MyApp, UserInsertionResult] = {

    import D._, L._

    info(s"Inserting User with email: '$e'").flatMap { _ =>
      queryByEmail(e).flatMap {
        case None =>
          insert(e, p).flatMap {
            case Success(id) =>
              info(s"User with email: '$e' inserted with id: '$id'").map { _ =>
                UserInserted(id)
              }
            case Failure(err) =>
              error(s"DB failed: '$err'", Some(err)).map { _ =>
                DBFailure(err)
              }
          }
        case Some(_) =>
          info(s"User with email: '$e' already exists!").map { _ =>
            AlreadyExists
          }
      }
    }
  }

  // HAVING MULTIPLE INTERPRETERS IS WHAT WE WANT TO HAVE, SO THIS IS ESSENTIAL
  object LogInterpreters {

    val logToId: Logging ~> Id = new (Logging ~> Id) {
      override def apply[A](fa: Logging[A]): Id[A] = fa match {
        case Debug(msg) => println(s"[DEBUG]: $msg")
        case Info(msg) => println(s"[INFO]: $msg")
        case Error(msg, thr) =>
          println(s"[ERROR]: $msg, $thr")
          thr.foreach(_.printStackTrace)
      }
    }

    val logToFuture: Logging ~> Future = new (Logging ~> Future) {
      override def apply[A](fa: Logging[A]): Future[A] =
        Future.successful(logToId.apply(fa))
    }
  }

  object WSInterpreters {

    val wsToId: WS ~> Id = new (WS ~> Id) {
      override def apply[A](fa: WS[A]): Id[A] = fa match {
        case Call(_, _) => WSResult(200, Map.empty, Array.empty)
      }
    }
  }

  //BOILERPLATE ...
  object AppInterpreters {

    import UserDBInterpreters._, LogInterpreters._, WSInterpreters._

    //Correct order has to be kept here - manual aligning
    //val interpreter: App ~> Id = logToId or loginDbToId
    val interpreter: MyApp ~> Id = userDBToId or logToId
    val interpreterWithWS: AppWithWS ~> Id = userDBToId or logToId or wsToId
  }

}

object RunInConsole {

  // :paste
  import multiple.MultipleAlgebras.saveUser
  import multiple.MultipleAlgebras.LoggingOperations._
  import multiple.MultipleAlgebras.UserDBOperations._
  import multiple.MultipleAlgebras.AppInterpreters

  val interpreter = AppInterpreters.interpreter

  def saveLglo = saveUser("lglo@gov.pl", "12345")

  def saveAbc = saveUser("abc@gov.pl", "dunno")
}

object RunFromCli extends App {

  import multiple.MultipleAlgebras._

  // import smart-constructors
  import LoggingOperations._
  import UserDBOperations._
  // import hand-made interpreter
  import AppInterpreters.interpreter

  val result = saveUser("lglo@gov.pl", "12345").foldMap(interpreter)
  println(result)
  
}