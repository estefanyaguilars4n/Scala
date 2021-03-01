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
  "La función and con la Lista (1,2,3,4)" should " debería devolver (1)" in {
    val list = List(1,2,3,4)
    List.head(list) shouldBe(1)
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

}