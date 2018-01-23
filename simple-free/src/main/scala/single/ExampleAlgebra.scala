package single

import cats.{Id, ~>}
import myfree.Free
import single.ExampleAlgebra.UserDB.{Insert, QueryByEmail}
import single.ExampleAlgebra.UserId
import single.UserInsertionResult._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

sealed trait UserInsertionResult

object UserInsertionResult {
  case class UserInserted(id: UserId) extends UserInsertionResult
  case object AlreadyExists extends UserInsertionResult
  case class DBFailure(err: Throwable) extends UserInsertionResult
}

object ExampleAlgebra {

  type Email = String
  type PasswordHash = String
  type UserId = Long

  trait UserDBRegular {
    def insert(e: Email, p: PasswordHash): Future[Try[UserId]]
    def queryByEmail(e: Email): Future[Option[(UserId, PasswordHash)]]
  }

  trait ExampleService {

    val db: UserDBRegular

    def saveUser(e: Email, p: PasswordHash): Future[UserInsertionResult] =
      db.queryByEmail(e).flatMap {
        case None =>
          db.insert(e, p).map {
            case Success(id) => UserInserted(id)
            case Failure(err) => DBFailure(err)
          }
        case Some(_) => Future.successful(AlreadyExists)
      }
  }

  sealed trait UserDB[A]

  object UserDB {
    final case class Insert(e: Email, p: PasswordHash) extends UserDB[Try[UserId]]
    final case class QueryByEmail(e: Email) extends UserDB[Option[(UserId, PasswordHash)]]
  }

  object UserDBPrograms {

    //Free.liftM and Free.pure can be seen as boilerplate
    def saveUser(e: Email, p: PasswordHash): Free[UserDB, UserInsertionResult] =
      Free.liftM(QueryByEmail(e)).flatMap {
        case None =>
          Free.liftM(Insert(e, p)).map {
            case Success(id) => UserInserted(id)
            case Failure(err) => DBFailure(err)
          }
        case Some(_) => Free.pure(AlreadyExists)
      }
  }

  object UserDBInterpreters {

    val userDBToId: UserDB ~> Id = new (UserDB ~> Id) {
      //I should have some database engine in use here...
      override def apply[A](fa: UserDB[A]): Id[A] = fa match {
        case Insert(e, p) =>
          // db.insert(User(email = Some(e), passwordHash = Some(p), ...).run
          if (e == "lglo@gov.pl")
            Failure(new Exception("Unique index or primary key violation"))
          else
            Success(42)

        case QueryByEmail(e) =>
          // db.query(_.email == e).map(row => (row.id, row.passwordHash)).headOption.run
          if (e == "lglo@gov.pl") Some((41, "S3CR3T")) else None
      }
    }
    val userDBToFuture: UserDB ~> Future = new (UserDB ~> Future) {
      override def apply[A](fa: UserDB[A]): Future[A] =
        Future(userDBToId(fa))
    }
  }

}

object RunMeInConsole {

  // :paste
  import single.ExampleAlgebra.UserDBInterpreters._
  import single.ExampleAlgebra.UserDBPrograms._
  import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  val save1 = saveUser("zonk@zonk.pl", "abcd")
  val save2 = saveUser("lglo@gov.pl", "omg")
  val save1F = save1.foldMap(userDBToFuture)
  val save2F = save2.foldMap(userDBToFuture)
}
