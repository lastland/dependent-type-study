package dependent.nat

trait Nat

class Z extends Nat

case class Succ[P <: Nat](n: P) extends Nat
