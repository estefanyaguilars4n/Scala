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
}