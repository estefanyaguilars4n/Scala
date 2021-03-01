package co.s4n.inmutable.nat

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

  object Nat{
    def fromNatToInt(nat:Nat):Int = nat match{
      case Cero => 0
      case Suc(nat) => 1 + fromNatToInt(nat)
    }
    def fromIntToNat(int:Int):Nat = int match {
      case 0 => Cero
      case n => Suc(fromIntToNat(n-1))
    }
    def addNat(nat1:Nat, nat2:Nat):Nat = {
      fromIntToNat(fromNatToInt(nat1) + fromNatToInt(nat2))
    }
    def prodNat(nat1:Nat, nat2:Nat):Nat = {
      fromIntToNat(fromNatToInt(nat1) * fromNatToInt(nat2))
    }
  }
