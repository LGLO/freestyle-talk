package io.scalac.frees.login

import cats.{Id, ~>}
import freestyle.free._
import io.scalac.frees.login.UserInsertionResult.{AlreadyExists, DBFailure, UserInserted}
import io.scalac.frees.login.types.{PasswordHash, UserEmail, UserId}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

sealed trait UserInsertionResult

object UserInsertionResult {

  case class UserInserted(id: UserId) extends UserInsertionResult

  case object AlreadyExists extends UserInsertionResult

  case class DBFailure(err: Throwable) extends UserInsertionResult

}

case class Row(id: UserId, email: UserEmail, password: PasswordHash)

@free trait UserDB {
  def insert(e: UserEmail, p: PasswordHash): FS[Try[UserId]]

  def queryByEmail(e: UserEmail): FS[Option[Row]]

  def saveUser(e: UserEmail, p: PasswordHash) =
    queryByEmail(e).flatMap {
      case None =>
        insert(e, p).map[UserInsertionResult] {
          case Success(id) => UserInserted(id)
          case Failure(err) => DBFailure(err)
        }
      case Some(_) => FreeS.pure(AlreadyExists: UserInsertionResult)
    }
}

object UserDBHandlers {

  implicit val id = new UserDB.Handler[Id] {

    var db = List.empty[Row]

    override def insert(e: UserEmail, p: PasswordHash): Id[Try[UserId]] =
      if(db.exists(_.email == e)) {
        Failure(new Exception("unique key violation"))
      } else {
        val id: UserId = db.headOption.map(_.id + 1).getOrElse(0)
        db = Row(id, e, p) :: db
        Success(id)
      }

    override def queryByEmail(e: UserEmail): Id[Option[Row]] =
      db.find(_.email == e)
  }

  implicit val future: UserDB.Op ~> Future = new (UserDB.Op ~> Future) {
    override def apply[A](fa: UserDB.Op[A]): Future[A] = Future.successful(id(fa))
  }
}

//Console doesn't work because of scalameta
object MainUserDB extends App {

  import UserDBHandlers._
  import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  val userDB = UserDB[UserDB.Op]
  val save1 = userDB.saveUser("zonk@zonk.pl", "abcd")
  val save2 = userDB.saveUser("lglo@gov.pl", "omg")
  println(save1.interpret[Id])
  println(save2.interpret[Id])
  println(save2.interpret[Id])
}
