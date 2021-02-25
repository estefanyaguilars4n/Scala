package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const [+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  def length[A](list:List[A]):Int = list match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
  def sum(ints: List[Int]):Int  = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }
  def product(ds: List[Double]):Double = ds match {
    case Nil => 1
    case Const(h,t) => h * product(t)
  }

  //Respuesta ejercicio 1: El resultado de la expresion match es x + y, es decir, 9

  //Ejercicio 2: La función recibe una lista de tipo Any y devuelve una lista con el tail de la lista insertada
  def tail[A](lista: List[A]):List[A] = lista match {
    case Nil => Nil
    case Const(h,t) => t
  }
   //Ejercicio 3:
  def head[A](lista: List[A]):Option[A] = lista match {
    //case Nil => None
    case Const(h,_) => Some(h)
  }
  //Ejercicio 4: La función recibe una lista de booleanos y devuelve un booleano true si todos los elementos
  // de la lista son true, de lo contrario devuelve un false
  def and(lst: List[Boolean]):Boolean = {
    @tailrec
    def andp(lst: List[Boolean], acumm: Boolean):Boolean = lst match {
      case Nil => acumm
      case Const(h,t) => andp(t,h && acumm)
    }
    andp(lst,true)
  }
  //Ejercicio 5: La funcion recibe una lista de booleanos y devuelve un booleano false si todos los elementos
  //de la lista son false, de lo contrario devuelve un booleano true
  def or(lst: List[Boolean]):Boolean = {
    @tailrec
    def orp(lst: List[Boolean],acum: Boolean):Boolean = lst match {
      case Nil => acum
      case Const(h,t) => orp(t,h || acum)
    }
    orp(lst,false)
  }
  //Ejercicio 6: La función recibe una lista de enteros y devuelve el mayor valor
  def max(lst: List[Int]):Int = {
    @tailrec
    def maxp(lst: List[Int],max: Int):Int = lst match{
      case Nil => max
      case Const(h,t) => maxp(t,if(h > max) h else max)
    }
    maxp(lst,Int.MaxValue)
  }
  //Ejercicio 7: La función recibe una lista de longs y devuelve el menor valor
  def min(lst: List[Long]):Long = {
    @tailrec
    def minp(lst: List[Long],min: Long):Long = lst match{
      case Nil => min
      case Const(h,t) => if (h < min) minp(t,h) else minp(t,min)
    }
    minp(lst,Long.MinValue)
  }
  //Ejercicio 8: La función recibe una lista de doubles y devuelve tanto el menor valor como el mayor
  def minMax(lst: List[Double]):(Double, Double) = {
    @tailrec
    def minp(lst: List[Double], min: Double):Double = lst match{
      case Nil => min
      case Const(h,t) => if (h < min) minp(t,h) else minp(t,min)
    }
    @tailrec
    def maxp(lst: List[Double], max:Double):Double = lst match {
      case Nil => max
      case Const(h,t) => if (h > max) maxp(t,h) else maxp(t,max)
    }
    (minp(lst,Double.MinValue),maxp(lst,Double.MaxValue))
  }
}