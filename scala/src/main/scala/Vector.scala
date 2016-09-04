package dependent

import scala.language.implicitConversions

import nat._

object Vector {
  trait Vec[+A, N <: Nat]

  case object Nil extends Vec[Nothing, Z]

  case class Cons[+A, N <: Nat](h: A, t: Vec[A, N]) extends Vec[A, Succ[N]]

  implicit def narrow[A, N <: Nat](v: Vec[A, Succ[N]]): Cons[A, N] = v match {
    case vc @ Cons(_, _) => vc
  }

  trait Rep[A, N <: Nat] {
    def apply(a: A): Vec[A, N]
  }

  implicit def zRep[A] = new Rep[A, Z] {
    def apply(a: A) = Nil
  }

  implicit def nRep[A, N <: Nat](implicit nat: Rep[A, N]) =
    new Rep[A, Succ[N]] {
      def apply(a: A) = Cons(a, rep[A, N](a))
    }

  def rep[A, N <: Nat](a: A)
    (implicit r: Rep[A, N]): Vec[A, N] = r(a)

  trait Concat[A, XS, YS] {
    type MN <: Nat
    type Out = Vec[A, MN]
    def apply(xs: XS, ys: YS): Out
  }

  implicit def nilConcat[A, N <: Nat] =
    new Concat[A, Vec[A, Z], Vec[A, N]] {
      type MN = N
      def apply(xs: Vec[A, Z], ys: Vec[A, N]): Out = ys
    }

  implicit def conConcat[A, M <: Nat, N <: Nat]
    (implicit tail: Concat[A, Vec[A, M], Vec[A, N]]) =
    new Concat[A, Vec[A, Succ[M]], Vec[A, N]] {
      type MN = Succ[tail.MN]
      def apply(xs: Vec[A, Succ[M]], ys: Vec[A, N]): Out =
        Cons(xs.h, app(xs.t, ys))
    }

  def app[A, N <: Nat, M <: Nat](xs: Vec[A, N], ys: Vec[A, M])
    (implicit concat: Concat[A, Vec[A, N], Vec[A, M]]): concat.Out =
    concat(xs, ys)
}
