package io.scalac.frees.login

import cats.{Id, ~>}
import freestyle.free._
import io.scalac.frees.login.UserInsertionResult.{AlreadyExists, DBFailure, UserInserted}
import io.scalac.frees.login.types.{PasswordHash, UserEmail}

import scala.concurrent.Future
import scala.util.{Failure, Success}

@module trait UserService {

  val logging: Logging
  val userDB: UserDB
  val ws: WS

  def saveUser(e: UserEmail, p: PasswordHash) = {

    import logging._, userDB._

    info(s"Inserting User with email: '$e'").flatMap { _ =>
      queryByEmail(e).flatMap {
        case None =>
          insert(e, p).flatMap {
            case Success(id) =>
              info(s"User with email: '$e' inserted with id: '$id'").map { _ =>
                UserInserted(id): UserInsertionResult
              }
            case Failure(err) =>
              error(s"DB failed: '$err'", Some(err)).map { _ =>
                DBFailure(err): UserInsertionResult
              }
          }
        case Some(_) =>
          warn(s"User $e already exists!").map { _ =>
            AlreadyExists: UserInsertionResult
          }
      }
    }
  }
}

class UserPrograms[F[_]]()(implicit D: UserService[F]) {
  type FS[A] = FreeS[F, A]

  def saveUser(e: UserEmail, p: PasswordHash) = {

    import D.logging._, D.userDB._

    info(s"Inserting User with email: '$e'").flatMap { _ =>
      queryByEmail(e).flatMap {
        case None =>
          insert(e, p).flatMap {
            case Success(id) =>
              info(s"User with email: '$e' inserted with id: '$id'").map { _ =>
                UserInserted(id): UserInsertionResult
              }
            case Failure(err) =>
              error(s"DB failed: '$err'", Some(err)).map { _ =>
                DBFailure(err): UserInsertionResult
              }
          }
        case Some(_) =>
          warn(s"User $e already exists!").map { _ =>
            AlreadyExists: UserInsertionResult
          }
      }
    }
  }
}

object MainUserService extends App {
  import freestyle.free.implicits._
  implicit val loggingIdHandler = LoggingHandlers.id
  implicit val userDbHandler = UserDBHandlers.id
  implicit val wsIdHandler = WSHandlers.wsIdHandler

  val us = UserService[UserService.Op]
  println(us.saveUser("zonk@zonk.pl", "abcd").interpret[Id])
  println(us.saveUser("lglo@gov.pl", "omg").interpret[Id])
  println(us.saveUser("lglo@gov.pl", "omg").interpret[Id])
  
  /*import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit def idToFuture[A, T[_]](id: T ~> Id): (T ~> Future) =
    new (T ~> Future){
      override def apply[A](fa: T[A]) = Future.successful(id(fa))
    }
  implicit val loggingFutureHandler = idToFuture(loggingIdHandler)
  implicit val userDBFutureHandler = idToFuture(userDbHandler)

  us.saveUser("lglo@gov.pl", "omg").interpret[Future].map(println)*/
}
