package co.s4n.inmutable.nat

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

  object Nat{
    /**Devuelve un entero luego de transformar el elemento natural recibido */
    def fromNatToInt(nat:Nat):Int = nat match{
      case Cero => 0
      case Suc(nat) => 1 + fromNatToInt(nat)
    }
    /**Devuelve un natural luego de transformar el elemento entero recibido */
    def fromIntToNat(int:Int):Nat = int match {
      case 0 => Cero
      case n => Suc(fromIntToNat(n-1))
    }
    /**Ejercicio 9. Devuelve un natural con la suma de los dos naturales recibidos */
    def addNat(nat1:Nat, nat2:Nat):Nat = {
      fromIntToNat(fromNatToInt(nat1) + fromNatToInt(nat2))
    }
    /**Ejercicio 10. Devuelve un natural con la productoria de los dos naturales recibidos */
    def prodNat(nat1:Nat, nat2:Nat):Nat = {
      fromIntToNat(fromNatToInt(nat1) * fromNatToInt(nat2))
    }
  }
