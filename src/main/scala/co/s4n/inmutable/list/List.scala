package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const [+A](h: A, t: List[A]) extends List[A]

object List {
  /**Recibe un conjunto de elementos separados por coma y lo convierte en una lista del tipo List */
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  /**Recibe una lista de tipo List y retorna la cantidad de elementos de esta */
  def length[A](list:List[A]):Int = list match {
    case Nil         => 0
    case Const(h, t) => 1 + length(t)
  }
  /**Recibe una lista de tipo List con elementos de tipo entero y retorna la sumatoria de todos sus elementos */
  def sum(ints:List[Int]):Int  = ints match {
    case Nil        => 0
    case Const(h,t) => h + sum(t)
  }
  /**Recibe una lista de tipo List con elementos de tipo double y retorna la productoria de todos sus elementos */
  def product(ds:List[Double]):Double = ds match {
    case Nil        => 1
    case Const(h,t) => h * product(t)
  }
  /**Respuesta ejercicio 1: El resultado de la expresion match es x + y, es decir, 9 */

  /** Ejercicio 2: Devuelve una lista con el tail de la lista recibida */
  def tail[A](lista:List[A]):List[A] = lista match {
    case Nil        => Nil
    case Const(h,t) => t
  }
   /**Ejercicio 3: Devuelve la cabeza o primer elemento de la lista recibida */
  def head[A](lista:List[A]):A = lista match {
    //case Nil => None
    case Const(h,_) => h
  }
  /**Ejercicio 4: Devuelve un booleano true si todos los elementos de la lista recibida son true */
  def and(lst:List[Boolean]):Boolean = {
    @tailrec
    def andp(lst:List[Boolean], acumm: Boolean):Boolean = lst match {
      case Nil        => acumm
      case Const(h,t) => andp(t,h && acumm)
    }
    andp(lst,true)
  }
  /**Ejercicio 5: Devuelve un booleano false si todos los elementos de la lista recibida son false */
  def or(lst:List[Boolean]):Boolean = {
    @tailrec
    def orp(lst:List[Boolean], acum: Boolean):Boolean = lst match {
      case Nil        => acum
      case Const(h,t) => orp(t,h || acum)
    }
    orp(lst,false)
  }
  /**Ejercicio 6: Devuelve el mayor valor de la lista de enteros recibida */
  def max(lst:List[Int]):Int = {
    @tailrec
    def maxp(lst:List[Int], max: Int):Int = lst match{
      case Nil        => max
      case Const(h,t) => maxp(t,if(h > max) h else max)
    }
    maxp(lst,Int.MinValue)
  }
  /**Ejercicio 7: Devuelve el menor valor de la lista de long recibida */
  def min(lst: List[Long]):Long = {
    @tailrec
    def minp(lst:List[Long], min: Long):Long = lst match{
      case Nil        => min
      case Const(h,t) => if (h < min) minp(t,h) else minp(t,min)
    }
    minp(lst,Long.MaxValue)
  }
  /**Ejercicio 8: Devuelve una tupla con el menor y el mayor valor de la lista de double recibida */
  def minMax(lst:List[Double]):(Double, Double) = {
    @tailrec
    def minMaxp(lst: List[Double], minmax: (Double, Double)):(Double, Double) = lst match{
      case Nil        => minmax
      case Const(h,t) => minMaxp(t,(if (h < minmax._1) h else minmax._1,if (h > minmax._2) h else minmax._2))
    }
    minMaxp(lst,(Double.MaxValue,Double.MinValue))
  }
  /**Devuelve una lista en la que el elemento recibido se añade a la cabeza de la lista recibida */
  def const[A](head:A, tail:List[A]):List[A] = Const(head,tail)
  /**Devuelve una lista en la que el elemento recibido se añade al final de la lista recibida */
  def addEnd[A](list:List[A], elem:A):List[A] = list match {
    case Nil        => Const(elem, Nil)
    case Const(h,t) => Const(h,addEnd(t, elem))
  }
  /**Devuelve una lista en la que concatena las dos listas recibidas */
  def append[A](list1:List[A], list2:List[A]):List[A] = (list1,list2) match {
    case (Nil,Nil)   => Nil
    case (list1,Nil) => list1
    case (Nil,list2) => list2
    case (Const(h,t),list2) => Const(h,append(t,list2))
  }
  /**Devuelve una lista de forma que elimina los n primeros elementos de la lista recibida */
  @tailrec
  def drop[A](n:Int, list:List[A]):List[A] = (n,list) match {
    case (0,list) => list
    case (n,Nil)  => Nil
    case (n,Const(h,t)) => drop(n-1,t)
  }
  /**Ejercicio 1.Devuelve una lista de forma que toma los n primeros elementos de la lista recibida */
  def take[A](n:Int, list:List[A]):List[A] = {
    @tailrec
    def takep[A](n: Int, list: List[A], accum: List[A]): List[A] = (n, list) match {
      case (0, _)   => accum
      case (_, Nil) => accum
      case (n, Const(h, t)) => takep(n - 1, t, addEnd(accum, head(list)))
    }
    takep(n, list, Nil)
  }
  /**Ejercicio 2. Devuelve una lista de forma que elimina el último elemento de la lista recibida */
  def init[A](list:List[A]):List[A] = {
    @tailrec
    def initp[A](list:List[A], aux:List[A]):List[A] = list match {
      case Const(_,Nil) => aux
      case Const(h,t)   => initp(t,addEnd(aux,h))
    }
    initp(list,Nil)
  }
  /**Ejercicio 3. Función que devuelve dos listas de forma que separa la lista recibida tomando los n primeros elementos
   * para la primera lista y el resto de elementos para la segunda */
  def split[A](n:Int, list:List[A]):(List[A],List[A]) =  {
    @tailrec
    def splitp[A](n:Int, lists:(List[A], List[A])):(List[A],List[A]) = (n,lists) match  {
      case (0,lists) => lists
      case (n,(_,_)) => splitp(n - 1,(addEnd(lists._1,head(lists._2)),tail(lists._2)))
    }
    splitp(n,(Nil,list))
  }
  /**Ejercicio 4. Devuelve una lista de tuplas con los elementos de las dos listas recibidas */
  def zip[A,B](list1:List[A], list2:List[B]):List[(A,B)] = {
    @tailrec
    def zipp[A,B](list1:List[A], list2:List[B], listZip:List[(A,B)]):List[(A,B)] = (list1,list2) match {
      case (Nil,_) => listZip
      case (_,Nil) => listZip
      case (Const(ha,ta),Const(hb,tb)) => zipp(ta,tb,addEnd(listZip,(ha,hb)))
    }
    zipp(list1,list2,Nil)
  }
  /**Ejercicio 5. Devuelve dos listas separando los elementos en las tuplas de la lista recibida, la primer lista con el
   * primer elemento de cada tupla y la segunda con el segundos elemento de cada tupla */
  def unzip[A,B](list:List[(A,B)]):(List[A],List[B]) = {
    @tailrec
    def unzipp (list:List[(A,B)], list1:List[A], list2:List[B]):(List[A],List[B]) = list match {
      case Nil        => (list1,list2)
      case Const(h,t) => unzipp(t,addEnd(list1,h._1),addEnd(list2,h._2))
    }
    unzipp(list,Nil,Nil)
  }
  /**Ejercicio 6. Devuelve una lista reversada de la lista recibida */
  def reverse[A](list:List[A]):List[A] = {
    @tailrec
    def reversep[A](list:List[A], aux:List[A]):List[A] = list match {
      case Nil        => aux
      case Const(h,t) => reversep(t,Const(h,aux))
    }
    reversep(list,Nil)
  }
  /**Ejercicio 7. Devuelve una lista de forma que añade el elemento recibido entre los elementos de la lista recibida */
  def intersperse[A](elem:A, list:List[A]):List[A] = {
    @tailrec
    def interspersep[A](elem:A, list:List[A], aux:List[A]):List[A] = list match {
      case Const(h,Nil) => addEnd(aux,h)
      case Const(h,t)   => interspersep(elem,t,addEnd(addEnd(aux,h),elem))
    }
    interspersep(elem,list,Nil)
  }
  /**Ejercicio 8. Devuelve una lista concatenando las listas pertenecientes a la lista inicial */
  def concat[A](list:List[List[A]]):List[A] = {
    @tailrec
    def concatp[A](list:List[List[A]], aux:List[A]):List[A] = list match {
      case Nil        => aux
      case Const(h,t) => concatp(t,append(aux,h))
    }
    concatp(list,Nil)
  }

  def dropWhile[A](list:List[A])(f:A => Boolean):List[A] = list match {
    case Const(h,t) if f(h) => dropWhile(t)(f)
    case _                  => list
  }
  /** FoldRight */
  def foldRight[A,B](list:List[A], z:B)(f:(A,B) => B):B = list match {
    case Nil        => z
    case Const(h,t) => f(h,foldRight(t,z)(f))
  }
  /**Ejercicio 13. Se realiza una copia de la lista introducida */
  /**Ejercicio 14. Devuelve la cantidad de elementos de la lista recibida */
  def lengthF[A](list:List[A]):Int = foldRight(list,0)((_,y) => 1 + y)
  /**Ejercicio 15. Devuelve un booleano true si todos los elementos de la lista recibida son true */
  def andF(list:List[Boolean]):Boolean = foldRight(list,true)(_&&_)
  /**Ejercicio 16. Devuelve una lista con los elementos de la lista recibida hasta el primer elemento que deje de
   * cumplir con el predicado recibido */
  def takeWhile[A](list:List[A])(p:A=>Boolean):List[A] = {
    def takeWhilep[A](list:List[A],aux:List[A])(p:A=>Boolean):List[A] = list match {
      case Const(h,t) if p(h) => takeWhilep(t,addEnd(aux,h))(p)
      case _                  => aux
    }
    takeWhilep(list,Nil)(p)
  }
  /**Ejercicio 17. Devuelve una lista con todos los elementos de la lista recibida que cumplan con el predicado
   * recibido */
  def filter[A](list:List[A])(p:A=>Boolean):List[A] =  foldRight(list,Nil:List[A])((x,y) => if (p(x)) Const(x,y) else y)
  /**Ejercicio 18. Devuelve dos listas separando los elementos en las tuplas de la lista recibida, la primer lista con
   * el primer elemento de cada tupla y la segunda con el segundos elemento de cada tupla */
  def unzipF[A,B](list:List[(A,B)]):(List[A],List[B]) =
    foldRight(list,(Nil:List[A],Nil:List[B]))((x,y) => (Const(x._1,y._1),Const(x._2,y._2)))
  /** FoldLeft */
  @tailrec
  def foldLeft[A,B](list:List[A], z:B)(f:(B,A)=>B):B = list match {
    case Const(h,t) => foldLeft(t,f(z,h))(f)
    case Nil        => z
  }
  /**Ejericio 19. Devuelve la cantidad de elementos de la lista recibida */
  def lengthL[A](list:List[A]):Int =  foldLeft(list,0)((x,_) => x + 1)
  /**Ejercicio 20. Devuelve un booleano true si todos los elementos de la lista recibida son true */
  def andL(list:List[Boolean]):Boolean =  foldLeft(list,true)(_&&_)
  /**Ejercicio 21. Devuelve una lista con los elementos de la lista recibida hasta el primer elemento que deje de
   * cumplir con el predicado recibido */
  def takeWhileL[A](list:List[A])(p:A=>Boolean):List[A] = {
    def f(b:(Boolean,List[A]),a:A):(Boolean,List[A]) = b match {
      case (true,list) => if (p(a)) (true,addEnd(list,a)) else (false,list)
      case (false,list) => b
    }
    foldLeft(list,(true,Nil:List[A]))(f)._2
  }
  /**Ejercicio 22. Devuelve una lista con todos los elementos de la lista recibida que cumplan con el predicado
   * recibido */
  def filterL[A](list:List[A])(p:A=>Boolean):List[A] =
    foldLeft(list,Nil:List[A])((y,x) => if(p(x)) addEnd(y,x) else y)
  /**Ejercicio 23. Devuelve dos listas separando los elementos en las tuplas de la lista recibida, la primer lista con
   * el primer elemento de cada tupla y la segunda con el segundos elemento de cada tupla */
  def unzipL[A,B](list:List[(A,B)]):(List[A],List[B]) =
    foldLeft(list,(Nil:List[A],Nil:List[B]))((y,x) => (addEnd(y._1,x._1),addEnd(y._2,x._2)))
  /**Map Generalizado */
  def mapGen[A,B](list:List[A])(f:A=>B):List[B] = list match{
    case Nil        => Nil
    case Const(h,t) => Const(f(h),mapGen(t)(f))
  }
  /** Sumar uno a cada elemento */
  def sumarUnoMap(list:List[Int]):List[Int] = mapGen(list)(_+1)
  /** Convertir de Int a String */
  def listInt2String(list:List[Int]):List[String] = mapGen(list)(_.toString)
  /** Map con foldRight */
  def map[A,B](list:List[A])(f:A=>B):List[B] = foldRight(list,Nil:List[B])((x,y)=> Const(f(x),y) )
 /** DropWhile con foldRight */

}