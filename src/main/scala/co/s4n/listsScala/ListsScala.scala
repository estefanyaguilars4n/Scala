package co.s4n.listsScala

import scala.annotation.tailrec

class ListsScala {

}
object ListsScala extends App {
  def subs[A](list: List[A]): List[List[A]] = {
    @tailrec
    def subsAux[A](list: List[A], iterator: Int, aux: List[List[A]]): List[List[A]] = iterator match {
      case -1 => aux
      case n  => subsAux(list, n - 1, list.combinations(n).toList ::: aux)
    }

    subsAux(list, list.length, Nil)
  }
  def barajar[A](a: A, list: List[A]): List[List[A]] = list match {
    case Nil     => List(List(a))
    case x :: xs => (a :: (x :: xs)) :: (barajar(a, xs)).map(x :: _)
  }
  def perms[A](list:List[A]):List[List[A]] = list match {
    case Nil     => List(Nil)
    case x :: xs => (perms(xs)).flatMap(barajar(x,_))
  }
  /**
   * Ejercicio 1. Devuelve el último elemento de la lista entregada. No puede recibir listas vacías.
   * @param list
   * @tparam A
   * @return
   */
  @tailrec
  def myLast[A](list:List[A]):A = list match{
    case head:: Nil   => head
    case head :: tail => myLast(tail)
  }

  /**
   * Ejercicio 2. Devuelve los dos últimos elementos de la lista entregada
   * @param list
   * @tparam A
   * @return
   */
  def myButLast[A](list:List[A]):List[A] = list match {
    case x :: y :: Nil => List(x,y)
    case x :: xs       => myButLast(xs)
  }

  /**
   * Devuelve la cola de la lista entergada
   * @param list
   * @tparam A
   * @return
   */
  def myTail[A](list:List[A]):List[A] = list match{
    case head :: Nil  => Nil
    case head :: tail => tail
  }

  /**
   * Ejercicio 3. Devuelve de la lista entregada el elemento de la posición indicada, siendo el primer elemento la
   * posición 1
   * @param list
   * @param spot
   * @tparam A
   * @return
   */
  def elementAt[A](list:List[A],spot:Int):A = (list,spot) match {
    case (x::xs,1) => x
    case (x::xs,n) => elementAt(xs,n-1)
  }

  /**
   * Ejercicio 4.1 Devuelve el número de elementos que tiene la lista entregada
   * @param list
   * @tparam A
   * @return
   */
  def myLength[A](list:List[A]):Int = list match {
    case Nil     => 0
    case x :: xs => 1 + myLength(xs)
  }

  /**
   * Ejercicio 4.2 Devuelve el número de elementos que tiene la lista entregada haciendo uso de la función foldRight
   * @param list
   * @tparam A
   * @return
   */
  def myLengthFR[A](list:List[A]):Int = list.foldRight(0)((_,x) => x + 1)

  /**
   * Ejercicio 4.3 Devuelve el número de elementos que tiene la lista entregada haciendo uso de la función foldLeft
   * @param list
   * @tparam A
   * @return
   */
  def myLengthFL[A](list:List[A]):Int = list.foldLeft(0)((x,_) => x + 1)

  /**
   * Ejercicio 5.1 Devuelve una lista hecha a partir de reversar la lista ingresada usando recursivdad de cola
   * @param list
   * @tparam A
   * @return
   */
  def myReverse[A](list:List[A]):List[A] = {
    @tailrec
    def myReversep[A](list:List[A],aux:List[A]):List[A] = list match {
      case Nil     => aux
      case x :: xs => myReversep(xs,x :: aux)
    }
    myReversep(list,Nil)
  }

  /**
   * Ejercicio 5.2 Devuelve una lista hecha a partir de reversar la lista ingresada
   * @param list
   * @tparam A
   * @return
   */
  def myReverse2[A](list:List[A]):List[A] = list match {
    case Nil          => Nil
    case head :: tail => myReverse2(tail) :+ head
  }

  /**
   * Devuelve una lista con todos los elementos de la lista entregada exceptuando el último usando recursividad de cola
   * @param list
   * @tparam A
   * @return
   */
  def myInit[A](list:List[A]):List[A] =  {
    @tailrec
    def myInitP[A](list:List[A],aux:List[A]):List[A] = list match {
      case head :: Nil  => aux
      case head :: tail => myInitP(tail,aux ::: List(head))
    }
    myInitP(list,Nil:List[A])
  }

  /**
   * Devuelve una lista con todos los elementos de la lista entregada exceptuando el último
   * @param list
   * @tparam A
   * @return
   */
  def myInit2[A](list:List[A]):List[A] = list match {
    case head :: Nil  => Nil
    case head :: tail => head :: myInit2(tail)
  }

  /**
   * Ejercicio 6. Devuelve un booleano true en caso de que la lista recibida sea paliíndroma, de lo contrario devuelve
   * un false
   * @param list
   * @tparam A
   * @return
   */
  def isPalindrome[A](list:List[A]):Boolean = list match{
    case head :: Nil        => true
    case head :: end :: Nil => head == end
    case head :: tail       => head == ListsScala.myLast(tail) && isPalindrome(ListsScala.myInit(tail))
  }
  /*sealed trait NestedList[+A]
  case class Element[A](element:A) extends NestedList[A]
  case class Const[A](list:List[NestedList[A]]) extends NestedList*/
  //def myFlatten[A]()
  /**
   * Ejercicio 8. Devuelve una lista de los elementos que posee la lista entregada pero sin repetidos consecutivos
   * @param list
   * @tparam A
   * @return
   */
  def compress[A](list:List[A]):List[A] = {
      def compressAux(list:List[A],aux:List[A]):List[A] = (list,aux) match {
        case (head1 :: tail,head2 :: Nil)                        => List(head2)
        case (head1 :: tail1,head2 :: tail2) if (head1 == head2) =>  compressAux(aux,tail2)
        case (head1 :: tail1,head2 :: tail2)                     => head1 :: compressAux(aux,tail2)
      }
    compressAux(list,ListsScala.myTail(list))
  }

  /**
   * Devuelve la cabeza de una lista. No puede recibir listas vacías
   * @param list
   * @tparam A
   * @return
   */
  def myHead[A](list:List[A]):A = list match{
    case (head :: tail) => head
  }

  /**
   * Ejercicio 9. Devuelve una lista que contiene sublistas cada una de estas con los elementos quese repiten
   * consecutivamente
   * @param list
   * @tparam A
   * @return
   */
  def pack[A](list:List[A]):List[List[A]] = {
    @tailrec
    def packAux(list:List[A],aux:List[A],aux2:List[List[A]]):List[List[A]] = list match {
      case Nil            => aux2 :+ aux
      case (head :: tail) => packAux(tail,if(head == myHead(aux)) aux :+ head else List(head),
        if(head == myHead(aux)) aux2 else aux2 :+ aux)
    }
    packAux(myTail(list),List(myHead(list)),Nil:List[List[A]])
  }

  /**
   * Ejercicio 10. Devuelve una lista con sublistas que contienen una tupla, donde el segundo valor es un elemento de
   * la lista recibida y el segundo un entero que contiene la cantidad de veces que el elemento se repite de forma
   * consecutiva
   * @param list
   * @tparam A
   * @return
   */
  def encode[A](list:List[A]):List[List[(Int,A)]] = {
    @tailrec
    def encodeAux(list:List[A],aux:List[(Int,A)],aux2:List[List[(Int,A)]]):List[List[(Int,A)]] = list match {
      case Nil            => aux2 :+ aux
      case (head :: tail) => encodeAux(tail,if(head == myHead(aux)._2) List((myHead(aux)._1 + 1,myHead(aux)._2))
        else List((1,head)), if(head == myHead(aux)._2) aux2 else aux2 :+ aux)
    }
    encodeAux(myTail(list),List((1,myHead(list))),Nil:List[List[(Int,A)]])
  }

  /**
   * Ejercicio 11. Devuelve una lista con sublistas que contienen una tupla, donde el segundo valor es un elemento de
   * la lista recibida y el segundo un entero que contiene la cantidad de veces que el elemento se repite de forma
   * consecutiva. Si el elemento no se repite de forma consecutiva se añade solo el elemento a la lista
   * @param list
   * @tparam A
   * @return
   */
  def encodeModified[A](list:List[A]):List[Any] = {
    @tailrec
    def encodeAux(list:List[A], aux:List[(Int,A)], aux2:List[Any]):List[Any] = list match {
      case Nil            =>  if(myHead(aux)._1 != 1) aux2 :+ aux else aux2 :+ myHead(aux)._2
      case (head :: tail) => encodeAux(tail,if(head == myHead(aux)._2) List((myHead(aux)._1 + 1,head))
        else List((1,head)), if(head == myHead(aux)._2) aux2 else if(myHead(aux)._1 == 1) aux2 :+ myHead(aux)._2
        else aux2 :+ aux)
    }
    encodeAux(myTail(list),List((1,myHead(list))),Nil:List[Any])
  }

  /**
   * Ejercicio 12. Devuelve una lista de tipo A en la que se decodifica el contenido de una lista cuyas sublistas
   * contienen tuplas con el elemento y el número de veces que este se repite consecutivamente
   * @param list
   * @tparam A
   * @return
   */
  def decodeModified[A](list:List[List[(Int,A)]]):List[A] = {
    @tailrec
    def decodeAux(list:List[List[(Int,A)]],aux:List[A]):List[A] = list match {
      case Nil => aux
      case List((0,element)) :: tail => decodeAux(tail,aux)
      case List((cant,element)) :: tail => decodeAux(List((cant - 1,element)) :: tail,aux :+ element)
    }
    decodeAux(list,Nil:List[A])
  }

  /**
   * Ejercicio 14. Devuelve una lista en la que se duplican los elementos de la lista recibida
   * @param list
   * @tparam A
   * @return
   */
  def duplicate[A](list:List[A]):List[A] = list match {
    case head :: Nil  => List(head, head)
    case head :: tail => head :: head :: duplicate(tail)
  }

  /**
   * Ejercicio 15. Devuelve una lista en la que se replican los elementos de la lista recibida una cantidad de veces
   * recibida
   * @param list
   * @param cant
   * @tparam A
   * @return
   */
  def replicate[A](list:List[A], cant:Int):List[A] = {
    @tailrec
    def replicateP[A](list:List[A],cant2:Int, aux:List[A]):List[A] = (cant2,list) match {
      case (n,Nil)          => aux
      case (0,head :: tail) => replicateP(tail,cant,aux)
      case (n,head :: tail) => replicateP(list,n-1,aux :+ head)
    }
    replicateP(list,cant,Nil:List[A])
  }

  /**
   * Ejercicio 16. Devuelve una lista en la que se elimina cada "every" elemento de la lista recibida
   * @param list
   * @param every
   * @tparam A
   * @return
   */
  def dropEvery[A](list:List[A],every:Int):List[A] = {
    @tailrec
    def dropEveryP[A](list:List[A],everyAux:Int,aux:List[A]):List[A] = (everyAux,list) match {
      case (_,Nil)          => aux
      case (1,head :: tail) => dropEveryP(tail,every,aux)
      case (n,head :: tail) => dropEveryP(tail,n-1,aux :+ head)
    }
    dropEveryP(list,every,Nil:List[A])
  }

  /**
   * Ejercicio 17. Devuelve dos listas partiendo la lista recibida de forma que la primera lista es la cantidad n hasta
   * la que se parte y la segunda lista contiene el resto de la lista inicial
   * @param list
   * @param n
   * @tparam A
   * @return
   */
  def mySplit[A](list:List[A],n:Int):(List[A],List[A]) = {
    @tailrec
    def mySplitAux(list: List[A],n:Int,aux:List[A]):(List[A],List[A]) = (n,list) match {
      case (_,Nil)          => (aux,list)
      case (0,_)            => (aux,list)
      case (a,head :: tail) => mySplitAux(tail,a-1,aux :+ head)
    }
    mySplitAux(list,n,Nil:List[A])
  }

  /**
   * Ejercicio 18. Devuelve una lista de elementos donde, recibiendo dos enteros, se usan esas posiciones en la lista
   * recibida para construirla
   * @param list
   * @param start
   * @param end
   * @tparam A
   * @return
   */
  def slice[A](list:List[A],start:Int,end:Int):List[A] = {
    @tailrec
    def sliceAux(list:List[A],start:Int,end:Int,actual:Int,aux:List[A]):List[A] = (actual,list) match {
      case (_,Nil)            => aux
      case (n,_) if(n==end+1) => aux
      case (n,head :: tail)   => sliceAux(tail,start,end,n + 1, if(n >= start) aux :+ head else aux)
    }
    sliceAux(list,start,end,1,Nil:List[A])
  }

  /**
   * Ejercicio 19. Rota la lista recibida una cantidad dada de espacios hacia la izquierda
   * @param list
   * @param spaces
   * @tparam A
   * @return
   */
  def rotate[A](list:List[A],spaces:Int):List[A] = {
    @tailrec
    def rotateAux(list:List[A],spaces:Int,aux:List[A]):List[A] = (spaces,list) match {
      case (0,list)         => list ::: aux
      case (n,head :: tail) => rotateAux(tail,if(n > 0) n - 1 else list.length + n - 1,aux :+ head)
    }
    rotateAux(list,spaces,Nil:List[A])
  }
  /**
   * Ejercicio 20. Devuelve una lista en la que se elimina el elemento indicado de la lista recibida
   * @param list
   * @param spot
   * @tparam A
   * @return
   */
  def removeAt[A](list:List[A],spot:Int):List[A] = {
    @tailrec
    def removeAtAux(list:List[A],spot:Int,aux:List[A]):List[A] = list match {
      case Nil => aux
      case head :: tail => removeAtAux(tail,spot - 1, if(spot != 1) aux :+ head else aux)
    }
    removeAtAux(list,spot,Nil:List[A])
  }

  /**
   * Ejercicio 21. Devuelve una lista que se compone de los elementos de la lista ingresada y un elemento extra añadido
   * en la posición indicada
   * @param list
   * @param element
   * @param spot
   * @tparam A
   * @return
   */
  def insertAt[A](list:List[A],element:A,spot:Int):List[A] = {
    @tailrec
    def insertAtAux(list:List[A],element:A,spot:Int,aux:List[A]):List[A] = list match {
      case Nil => aux
      case head :: tail => insertAtAux(tail,element,spot - 1, if(spot != 1) aux :+ head else aux :+ element :+ head)
    }
    insertAtAux(list,element,spot,Nil:List[A])
  }

  /**
   * Ejercicio 22. Devuelve una lista que, a partir de los dos enteros de inicio y fin recibidos, esta compuesta por
   * ese rango de numeros
   * @param start
   * @param end
   * @return
   */
  def range(start:Int,end:Int):List[Int] =  if (start == end +1 ) Nil else start :: range(start + 1,end)
}