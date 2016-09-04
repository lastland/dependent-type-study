package dependent.nat

trait Nat

class Z extends Nat {
  override def toString = "Z"
}

case class Succ[P <: Nat](n: P) extends Nat {
  def pred: P = n
}

object Nat {
  val z = new Z

  trait SPlus[M <: Nat, N <: Nat] {
    type MN <: Nat
    type Out = MN
    def apply(m: M, n: N): Out
  }

  implicit def zPlus[N <: Nat] = new SPlus[Z, N] {
    type MN = N
    def apply(m: Z, n: N): Out = n
  }

  implicit def nPlus[M <: Nat, N <: Nat]
    (implicit pred: SPlus[M, N]) =
    new SPlus[Succ[M], N] {
      type MN = Succ[pred.MN]
      def apply(m: Succ[M], n: N): Out = Succ(plus(m.pred, n))
    }

  def plus[M <: Nat, N <: Nat](m: M, n: N)
    (implicit splus: SPlus[M, N]): splus.Out =
    splus(m, n)
}
