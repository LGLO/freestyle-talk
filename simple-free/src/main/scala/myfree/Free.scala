package myfree

import cats.{Monad, ~>}
import myfree.Free.{Bind, Pure, Suspend}

// 1
sealed trait Free[F[_], A] {
  // 6 - the heart of monad
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)
  // 8 - G is a concrete monad and we know how to use it
  // ~> is function that works on type constructors
  def foldMap[G[_] : Monad](nat: F ~> G): G[A] = this match {
    case Pure(a) => Monad[G].pure(a)
    case Suspend(fa) => nat(fa)
    case Bind(target, f) => Monad[G].flatMap(target.foldMap(nat))(a => f(a).foldMap(nat))
  }
  // 9 - not strictly required - added to have for-comprehension
  def map[B](f: A => B): Free[F, B] = this.flatMap(a => Free.pure(f(a)))
}

object Free {
  // 2 - turn value to part of algebra
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  // 4 - because it's handy
  def liftM[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)
  // 3 - to define #pure
  final case class Pure[F[_], A](a: A) extends Free[F, A]
  // 5 - to define #liftM
  final case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  // 7 - to define #flatMap
  final case class Bind[F[_], E, A](target: Free[F, E], f: E => Free[F, A]) extends Free[F, A]
}
