package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const [+A](h: A, t: List[A]) extends List[A]

object List {
  //Función que recibe un conjunto de elementos separados por coma y lo convierte en una lista del tipo List
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  //Función que recibe una lista de tipo List y retorna la cantidad de elementos de esta
  def length[A](list:List[A]):Int = list match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
  //Función que recibe una lista de tipo List con elementos de tipo entero y retorna la sumatoria de todos sus elementos
  def sum(ints:List[Int]):Int  = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }
  //Función que recibe una lista de tipo List con elementos de tipo double y retorna la productoria de todos sus elementos
  def product(ds:List[Double]):Double = ds match {
    case Nil => 1
    case Const(h,t) => h * product(t)
  }
  //Respuesta ejercicio 1: El resultado de la expresion match es x + y, es decir, 9
  //Ejercicio 2: La función recibe una lista de tipo Any y devuelve una lista con el tail de la lista insertada
  def tail[A](lista:List[A]):List[A] = lista match {
    case Nil => Nil
    case Const(h,t) => t
  }
   //Ejercicio 3: La función recibe una lista de tipo Any y devuelve la cabeza o primer elemento de la lista insertada
  def head[A](lista:List[A]):A = lista match {
    //case Nil => None
    case Const(h,_) => h
  }
  //Ejercicio 4: La función recibe una lista de booleanos y devuelve un booleano true si todos los elementos
  // de la lista son true, de lo contrario devuelve un false
  def and(lst:List[Boolean]):Boolean = {
    @tailrec
    def andp(lst:List[Boolean], acumm: Boolean):Boolean = lst match {
      case Nil => acumm
      case Const(h,t) => andp(t,h && acumm)
    }
    andp(lst,true)
  }
  //Ejercicio 5: La funcion recibe una lista de booleanos y devuelve un booleano false si todos los elementos
  //de la lista son false, de lo contrario devuelve un booleano true
  def or(lst:List[Boolean]):Boolean = {
    @tailrec
    def orp(lst:List[Boolean], acum: Boolean):Boolean = lst match {
      case Nil => acum
      case Const(h,t) => orp(t,h || acum)
    }
    orp(lst,false)
  }
  //Ejercicio 6: La función recibe una lista de enteros y devuelve el mayor valor
  def max(lst:List[Int]):Int = {
    @tailrec
    def maxp(lst:List[Int], max: Int):Int = lst match{
      case Nil => max
      case Const(h,t) => maxp(t,if(h > max) h else max)
    }
    maxp(lst,Int.MaxValue)
  }
  //Ejercicio 7: La función recibe una lista de longs y devuelve el menor valor
  def min(lst: List[Long]):Long = {
    @tailrec
    def minp(lst:List[Long], min: Long):Long = lst match{
      case Nil => min
      case Const(h,t) => if (h < min) minp(t,h) else minp(t,min)
    }
    minp(lst,Long.MinValue)
  }
  //Ejercicio 8: La función recibe una lista de double y devuelve tanto el menor valor como el mayor
  def minMax(lst:List[Double]):(Double, Double) = {
    @tailrec
    def minMaxp(lst: List[Double], minmax: (Double, Double)):(Double, Double) = lst match{
      case Nil => minmax
      case Const(h,t) => minMaxp(t,(if (h < minmax._1) h else minmax._1,if (h > minmax._2) h else minmax._2))
    }
    minMaxp(lst,(Double.MaxValue,Double.MinValue))
  }
  //Función que recibe un elemento y una lista de cualquier tipo, pero ambos del mismo tipo, y adiciona el elemento
  // a la cabeza de la lista
  def const[A](head:A, tail:List[A]):List[A] = Const(head,tail)
  //Función que recibe un elemento y una lista de cualquier tipo, pero ambos del mismo tipo, y adiciona el elemento
  // al final de la lista
  def addEnd[A](list:List[A], elem:A):List[A] = list match {
    case Nil => Const(elem, Nil)
    case Const(h,t) => Const(h,addEnd(t, elem))
  }
  //Función que recibe tos listas de cualquier tipo, pero ambas del mismo tipo, y las concatena
  def append[A](list1:List[A], list2:List[A]):List[A] = (list1,list2) match {
    case (Nil,Nil) => Nil
    case (list1,Nil) => list1
    case (Nil,list2) => list2
    case (Const(h,t),list2) => Const(h,append(t,list2))
  }
  //Función que devuelve una lista de forma que elimina los n primeros elementos de la lista recibida
  @tailrec
  def drop[A](n:Int, list:List[A]):List[A] = (n,list) match {
    case (0,list) => list
    case (n,Nil) => Nil
    case (n,Const(h,t)) => drop(n-1,t)
  }
  //Ejercicio 1. Función que devuelve una lista de forma que toma los n primeros elementos de la lista recibida
  def take[A](n:Int, list:List[A]):List[A] = {
    @tailrec
    def takep[A](n: Int, list: List[A], accum: List[A]): List[A] = (n, list) match {
      case (0, _) => accum
      case (_, Nil) => accum
      case (n, Const(h, t)) => takep(n - 1, t, addEnd(accum, head(list)))
    }
    takep(n, list, Nil)
  }
  //Ejercicio 2. Función que devuelve una lista de forma que elimina el último elemento de la lista recibida
  def init[A](list:List[A]):List[A] = {
    @tailrec
    def initp[A](list:List[A], aux:List[A]):List[A] = list match {
      case Const(_,Nil) => aux
      case Const(h,t) => initp(t,addEnd(aux,h))
    }
    initp(list,Nil)
  }
  //Ejercicio 3. Función que devuelve dos listas de forma que separa la lista recibida tomando los n primeros elementos
  // para la primera lista y el resto de la lista para la segunda
  def split[A](n:Int, list:List[A]):(List[A],List[A]) =  {
    @tailrec
    def splitp[A](n:Int, lists:(List[A], List[A])):(List[A],List[A]) = (n,lists) match  {
      case (0,lists) => lists
      case (n,(_,_)) => splitp(n - 1,(addEnd(lists._1,head(lists._2)),tail(lists._2)))
    }
    splitp(n,(Nil,list))
  }
  //Ejercicio 4. Función que recibe dos listas y devuelve una lista con tupas de los elementos de ambas listas
  def zip[A,B](list1:List[A], list2:List[B]):List[(A,B)] = {
    @tailrec
    def zipp[A,B](list1:List[A], list2:List[B], listZip:List[(A,B)]):List[(A,B)] = (list1,list2) match {
      case (Nil,_) => listZip
      case (_,Nil) => listZip
      case (Const(ha,ta),Const(hb,tb)) => zipp(ta,tb,addEnd(listZip,(ha,hb)))
    }
    zipp(list1,list2,Nil)
  }
  //Ejercicio 5. Función que recibe una lista de tuplas y devuelve dos listas, la primera con los primeros elementos de
  // cada tupla y la segunda con los segundos elementos de cada tupla
  def unzip[A,B](list:List[(A,B)]):(List[A],List[B]) = {
    @tailrec
    def unzipp (list:List[(A,B)], list1:List[A], list2:List[B]):(List[A],List[B]) = list match {
      case Nil => (list1,list2)
      case Const(h,t) => unzipp(t,addEnd(list1,h._1),addEnd(list2,h._2))
    }
    unzipp(list,Nil,Nil)
  }
  //Ejercicio 6. Función que recibe una lista y devuelve una lista reversada de la recibida
  def reverse[A](list:List[A]):List[A] = {
    @tailrec
    def reversep[A](list:List[A], aux:List[A]):List[A] = list match {
      case Nil => aux
      case Const(h,t) => reversep(t,const(h,aux))
    }
    reversep(list,Nil)
  }
  //Ejercicio 7. Función que recibe una lista de cualquier tipo y un elemento del mismo tipo de la lista y añade este
  // elemento entre los elementos que pertenecen a la lista
  def intersperse[A](elem:A, list:List[A]):List[A] = {
    @tailrec
    def interspersep[A](elem:A, list:List[A], aux:List[A]):List[A] = list match {
      case Const(h,Nil) => addEnd(aux,h)
      case Const(h,t) => interspersep(elem,t,addEnd(addEnd(aux,h),elem))
    }
    interspersep(elem,list,Nil)
  }
  //Ejercicio 8. Función que recibe una lista de listas y devuelve una sola lista concatenando las listas que
  // pertenecían a la lista inicial
  def concat[A](list:List[List[A]]):List[A] = {
    @tailrec
    def concatp[A](list:List[List[A]], aux:List[A]):List[A] = list match {
      case Nil => aux
      case Const(h,t) => concatp(t,append(aux,h))
    }
    concatp(list,Nil)
  }
}