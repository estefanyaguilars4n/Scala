package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  "La función tail con la Lista (1,2,3,4)" should " debería devolver (2,3,4)" in {
    val list = List(1,2,3,4)
    List.tail(list) shouldBe(Const(2,Const(3,Const(4,Nil))))
  }
  "La función head con la Lista (1,2,3,4)" should " debería devolver (1)" in {
    val list = List(1,2,3,4)
    List.head(list) shouldBe(1)
  }
  "La función and con la Lista (true,false,true,true)" should " debería devolver (false)" in {
    val list = List(true,false,true,true)
    List.and(list) shouldBe(false)
  }
  "La función and con la Lista (true,true,true,true)" should " debería devolver (true)" in {
    val list = List(true,true,true,true)
    List.and(list) shouldBe(true)
  }
  "La función or con la lista (false,true,false,false)" should "debería devolver (true)" in {
    val list = List(false,true,false,false)
    List.or(list) shouldBe(true)
  }
  "La función or con la lista (false,false,false)" should "debería devolver (false)" in {
    val list = List(false,false,false)
    List.or(list) shouldBe(false)
  }
  "La función max enviando la lista (15,6,8,32,10)" should "debería devolver (32)" in {
    val list = List(15,6,8,32,10)
    List.max(list) shouldBe(32)
  }
  "La función min enviando la lista (15,6,8,32,10)" should "debería devolver (6)" in {
    val list = List(15L,6L,8L,32L,10L)
    List.min(list) shouldBe(6L)
  }
  "La función minMax enviando la lista (15.0,6.5,8.4,32.1,10.7)" should "debería devolver (6.5,32.1)" in {
    val list = List(15.0,6.5,8.4,32.1,10.7)
    List.minMax(list) shouldBe((6.5,32.1))
  }
  "La función const enviando el elemento (true) y la lista (false,true,true)" should "debería devolver la lista " +
    "(true,false,true,true)" in {
    val list = List(false,true,true)
    val head = true
    val listExit = List(true,false,true,true)
    List.const(head,list) shouldBe(listExit)
  }
  "La función addEnd enviando el elemento (false) y la lista (false,true,true)" should "debería devolver la lista " +
    "(false,true,true,false)" in {
    val list = List(false,true,true)
    val element = false
    val listExit = List(false,true,true,false)
    List.addEnd(list,element) shouldBe(listExit)
  }
  "La función take con la Lista (1,2,3,4) y el n 3" should " debería devolver (1,2,3)" in {
    val list = List(1,2,3,4)
    List.take(3,list) shouldBe(Const(1,Const(2,Const(3,Nil))))
  }
  "La función take con la Lista (1,2,3,4) y el n 0" should " debería devolver Nil" in {
    val list = List(1,2,3,4)
    List.take(0,list) shouldBe(Nil)
  }
  "La función take con la Lista (1,2,3,4) y el n 6" should " debería devolver (1,2,3,4)" in {
    val list = List(1,2,3,4)
    List.take(6,list) shouldBe(Const(1,Const(2,Const(3,Const(4,Nil)))))
  }
  "La función init con la Lista (1,2,3,4) " should " debería devolver (1,2,3)" in {
    val list = List(1,2,3,4)
    List.init(list) shouldBe(Const(1,Const(2,Const(3,Nil))))
  }
  "La función Split con la Lista (1,2,3,4) y n 2" should " debería devolver ((1,2),(3,4))" in {
    val list = List(1,2,3,4)
    List.split(2,list) shouldBe(((Const(1,Const(2,Nil))),(Const(3,Const(4,Nil)))))
  }
  "La función zip al enviarle la listas (1,2,3) y (true, false, true)" should "debería devolver la lista de tipo " +
    "(A,B) ((1,true),(2,false),(3,true))" in {
    val list1 = List(1,2,3)
    val list2 = List(true,false,true)
    List.zip(list1,list2) shouldBe(Const((1,true),Const((2,false),Const((3,true),Nil))))
  }
  "La función zip al enviarle la listas (1,2,3,4) y (true, true, false)" should "debería devolver la lista de tipo " +
    "(A,B) ((1,true),(2,true),(3,false))" in {
    val list1 = List(1,2,3,4)
    val list2 = List(true,true,false)
    List.zip(list1,list2) shouldBe(Const((1,true),Const((2,true),Const((3,false),Nil))))
  }
  "La función unzip al enviarle la lista ((1,\"a\"),(2,\"b\"),(3,\"c\"))" should "debería devolver las listas (1,2,3)" +
    " y (\"a\",\"b\",\"c\")" in {
    val list = List((1,"a"),(2,"b"),(3,"c"))
    List.unzip(list) shouldBe((Const(1,Const(2,Const(3,Nil))),Const("a",Const("b",Const("c",Nil)))))
  }
  "La función reverse al enviarle la lista (6,7,8,9)" should "debería devolver la lista (9,8,7,6)" in {
    val list = List(6,7,8,9)
    val listExit = List(9,8,7,6)
    List.reverse(list) shouldBe(listExit)
  }
  "La función intersperse al enviarle la lista (1,2,3) y enviarle el elemento 6" should "debería devolver la lista " +
    "(1,6,2,6,3)" in {
    val list = List(1,2,3)
    val listExit = List(1,6,2,6,3)
    List.intersperse(6,list) shouldBe(listExit)
  }
  "La función concat al enviarle la lista (List(1,2),List(3,4),List(5,6))" should "debería devolver la lista " +
    "(1,2,3,4,5,6)" in {
    val list1 = List(1,2)
    val list2 = List(3,4)
    val list3 = List(5,6)
    val list = List(list1,list2,list3)
    val listExit = List(1,2,3,4,5,6)
    List.concat(list) shouldBe(listExit)
  }
  "La función dropWhile al enviarle la lista (1,2,3,4,5,6) y la fución anónima (< 3)" should "debería devolver la lista " +
    "(3,4,5,6)" in {
    val list = List(1,2,3,4,5,6)
    val listExit = List(3,4,5,6)
    List.dropWhile(list)(y => y < 3) shouldBe(listExit)
  }
  "La función foldRight enviando la lista ( 9 L , 6 L , 7 L ) y Nil" should "debería devolver la lista " +
    "( 9 L , 6 L , 7 L )" in {
    val list = List(9L,6L,7L)
    List.foldRight(list,Nil:List[Long])(Const(_,_)) shouldBe(list)
  }
  "La función lengthF enviando la lista (1,2,3,4,5,6) y 0" should "debería devolver (6)" in {
    val list = List(1,2,3,4,5,6)
    List.lengthF(list) shouldBe(6)
  }
  "La función andF con la Lista (true,false,true,true)" should " debería devolver (false)" in {
    val list = List(true,false,true,true)
    List.andF(list) shouldBe(false)
  }
  "La función andF con la Lista (true,true,true,true)" should " debería devolver (true)" in {
    val list = List(true,true,true,true)
    List.andF(list) shouldBe(true)
  }
  "La función takeWhile al enviarle la lista (1,2,3,4,5,6,0) y la fución anónima (< 4)" should "debería devolver la lista " +
    "(1,2,3)" in {
    val list = List(1,2,3,4,5,6,0)
    val listExit = List(1,2,3)
    List.takeWhile(list)(x => x < 4) shouldBe(listExit)
  }
  "La función filter al enviarle la lista (1,10,4,8,3,7,2) y la función anónima (< 5)" should "debería devolver la " +
    "lista (1,4,3,2)" in {
    val list = List(1,10,4,8,3,7,2)
    val listExit = List(1,4,3,2)
    List.filter(list)(x => x < 5) shouldBe(listExit)
  }
  "La función unzipF al enviarle la lista ((true,5),(false,6),(false,7))" should "debería devolver las listas " +
    "(true,false,false) y (5,6,7) " in {
    val list = List((true,5),(false,6),(false,7))
    val listExit1 = List(true,false,false)
    val listExit2 = List(5,6,7)
    List.unzipF(list) shouldBe((listExit1,listExit2))
  }
  "La función lengthL al enviarle la lista (1,10,4,8,3,7,2)" should "debería devolver (7)" in {
    val list = List(1,10,4,8,3,7,2)
    List.lengthL(list) shouldBe(7)
  }
  "La función andL con la Lista (true,false,true,true)" should " debería devolver (false)" in {
    val list = List(true,false,true,true)
    List.andL(list) shouldBe(false)
  }
  "La función andL con la Lista (true,true,true,true)" should " debería devolver (true)" in {
    val list = List(true,true,true,true)
    List.andL(list) shouldBe(true)
  }
  "La función takeWhileL con la lista (1,2,3,4,5,6) y la función anónima (< 4)" should "debería devolver la " +
    "lista (1,2,3)" in {
    val list = List(1,2,3,4,5,6)
    val listExit = List(1,2,3)
    List.takeWhileL(list)(_ < 4) shouldBe(listExit)
  }
  "La función filterL al enviarle la lista (1,10,4,8,3,7,2) y la función anónima (< 5)" should "debería devolver la " +
    "lista (1,4,3,2)" in {
    val list = List(1,10,4,8,3,7,2)
    val listExit = List(1,4,3,2)
    List.filterL(list)(_ < 5) shouldBe(listExit)
  }
  "La función unzipL al enviarle la lista ((true,5),(false,6),(false,7))" should "debería devolver las listas " +
    "(true,false,false) y (5,6,7) " in {
    val list = List((true,5),(false,6),(false,7))
    val listExit1 = List(true,false,false)
    val listExit2 = List(5,6,7)
    List.unzipL(list) shouldBe((listExit1,listExit2))
  }
}