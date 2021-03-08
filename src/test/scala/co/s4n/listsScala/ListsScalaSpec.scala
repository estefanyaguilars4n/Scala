package co.s4n.listsScala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListsScalaSpec extends AnyFlatSpec with Matchers {
  "myReverse con List(1,2,3)" should "List(3,2,1)" in {
    val list = List(1,2,3)
    val listExit = List(3,2,1)
    ListsScala.myReverse(list) shouldBe(listExit)
  }
  "myReverse2 con List(1,2,3)" should "List(3,2,1)" in {
    val list = List(1,2,3)
    val listExit = List(3,2,1)
    ListsScala.myReverse2(list) shouldBe(listExit)
  }
  "myInit con List(1,2,3,4,5)" should "List(1,2,3,4)" in {
    val list = List(1,2,3,4,5)
    val listExit = List(1,2,3,4)
    ListsScala.myInit2(list) shouldBe(listExit)
  }
  "isPalindrome con List(1,2,1)" should "true" in {
    val list = List(1,2,1)
    ListsScala.isPalindrome(list) shouldBe(true)
  }
  "isPalindrome con List(1,2,2,1)" should "true" in {
    val list = List(1,2,2,1)
    ListsScala.isPalindrome(list) shouldBe(true)
  }
  "isPalindrome con List(1,2,3)" should "false" in {
    val list = List(1,2,3)
    ListsScala.isPalindrome(list) shouldBe(false)
  }
  "compress con List(1,1,1,2,2)" should "List(1,2)" in {
    val list = List(1,1,1,2,2)
    val listExit = List(1,2)
    ListsScala.compress(list) shouldBe(listExit)
  }
  "compress con List(3,3,3,5,5,3,3,2,2)" should "List(3,5,3,2)" in {
    val list = List(3,3,3,5,5,3,3,2,2)
    val listExit = List(3,5,3,2)
    ListsScala.compress(list) shouldBe(listExit)
  }
  "pack con List(3,3,3,5,5,3,3,2,2)" should "List(List(3,3,3),List(5,5),List(3,3),List(2,2))" in {
    val list = List(3,3,3,5,5,3,3,2,2)
    val listExit = List(List(3,3,3),List(5,5),List(3,3),List(2,2))
    ListsScala.pack(list) shouldBe(listExit)
  }
  "pack con List(1,1,2,3,3,3)" should "List(List(1,1),List(2),List(3,3,3))" in {
    val list = List(1,1,2,3,3,3)
    val listExit = List(List(1,1),List(2),List(3,3,3))
    ListsScala.pack(list) shouldBe(listExit)
  }
  "encode con List(3,3,3,5,5,3,3,2,2)" should "List(List((3,3)),List((2,5)),List((2,3)),List((2,2)))" in {
    val list = List(3,3,3,5,5,3,3,2,2)
    val listExit = List(List((3,3)),List((2,5)),List((2,3)),List((2,2)))
    ListsScala.encode(list) shouldBe(listExit)
  }
  "encode con List(1,1,2,3,3,3)" should "List(List((2,1)),List((1,2)),List((3,3)))" in {
    val list = List(1,1,2,3,3,3)
    val listExit = List(List((2,1)),List((1,2)),List((3,3)))
    ListsScala.encode(list) shouldBe(listExit)
  }
  "encodeModified con List(1,1,2,3,3,3)" should "List(List((2,1)),2,List((3,3)))" in {
    val list = List(1,1,2,3,3,3)
    val listExit = List(List((2,1)),2,List((3,3)))
    ListsScala.encodeModified(list) shouldBe(listExit)
  }
  "encodeModified con List('a','a','b','c','d','d','d','e')" should "List(List((2,'a')),'b','c',List((2,d)),'e')" in {
    val list = List('a','a','b','c','d','d','d','e')
    val listExit = List(List((2,'a')),'b','c',List((3,'d')),'e')
    ListsScala.encodeModified(list) shouldBe(listExit)
  }
  "decodeModified con List(List((2,1)),List((1,2)),List((3,3)))" should "List(1,1,2,3,3,3)" in {
    val list = List(List((2,1)),List((1,2)),List((3,3)))
    val listExit = List(1,1,2,3,3,3)
    ListsScala.decodeModified(list) shouldBe(listExit)
  }
  "decodeModified con List(List((3,3)),List((2,5)),List((2,3)),List((2,2)))" should "List(3,3,3,5,5,3,3,2,2)" in {
    val list = List(List((3,3)),List((2,5)),List((2,3)),List((2,2)))
    val listExit = List(3,3,3,5,5,3,3,2,2)
    ListsScala.decodeModified(list) shouldBe(listExit)
  }
  "duplicate con List(3,5,5,2)" should "List(3,3,5,5,5,5,2,2)" in {
    val list = List(3,5,5,2)
    val listExit = List(3,3,5,5,5,5,2,2)
    ListsScala.duplicate(list) shouldBe(listExit)
  }
  "replicate con List(1,2,3) y num 3" should "List(1,1,1,2,2,2,3,3,3)" in {
    val list = List(1,2,3)
    val listExit = List(1,1,1,2,2,2,3,3,3)
    ListsScala.replicate(list,3) shouldBe(listExit)
  }
  "dropEvery con List(1,2,3,4,5,6) y every 3" should "List(1,2,4,5)" in {
    val list = List(1,2,3,4,5,6)
    val listExit = List(1,2,4,5)
    ListsScala.dropEvery(list,3) shouldBe(listExit)
  }
  "mySplit con List('a','a','b','c','d','d','d','e') y 4" should "(List('a','a','b','c'),List('d','d','d','e'))" in {
    val list = List('a','a','b','c','d','d','d','e')
    val tuplaExit = (List('a','a','b','c'),List('d','d','d','e'))
    ListsScala.mySplit(list,4) shouldBe(tuplaExit)
  }
  "mySplit con List('a','a','b','c','d','d','d','e') y 0" should "(Nil,List('a','a','b','c','d','d','d','e'))" in {
    val list = List('a','a','b','c','d','d','d','e')
    val tuplaExit = (Nil,List('a','a','b','c','d','d','d','e'))
    ListsScala.mySplit(list,0) shouldBe(tuplaExit)
  }
  "slice con List('a','a','b','c','d','d','d','e'), 3 y 7" should "List('b','c','d','d','d')" in {
    val list = List('a','a','b','c','d','d','d','e')
    val listExit = List('b','c','d','d','d')
    ListsScala.slice(list,3,7) shouldBe(listExit)
  }
  "slice con List('a','a','b','c','d','d','d','e'), 2 y 10" should "List('a','b','c','d','d','d','e')" in {
    val list = List('a','a','b','c','d','d','d','e')
    val listExit = List('a','b','c','d','d','d','e')
    ListsScala.slice(list,2,10) shouldBe(listExit)
  }
  "rotate con List('a','a','b','c','d','d','d','e') y 5" should "List('d','d','e','a','a','b','c','d')" in {
    val list = List('a','a','b','c','d','d','d','e')
    val listExit = List('d','d','e','a','a','b','c','d')
    ListsScala.rotate(list,5) shouldBe(listExit)
  }
  "rotate con List('a','a','b','c','d','d','d','e') y -3" should "List('d','d','e','a','a','b','c','d')" in {
    val list = List('a','a','b','c','d','d','d','e')
    val listExit = List('d','d','e','a','a','b','c','d')
    ListsScala.rotate(list,-3) shouldBe(listExit)
  }
  "removeAt con List(1,2,3,4,5,6,7,8,9), 6" should "List(1,2,3,4,5,7,8,9)" in {
    val list = List(1,2,3,4,5,6,7,8,9)
    val listExit = List(1,2,3,4,5,7,8,9)
    ListsScala.removeAt(list,6) shouldBe(listExit)
  }
  "removeAt con List(1,2,3,4,5,6,7,8,9), 10" should "List(1,2,3,4,5,6,7,8,9)" in {
    val list = List(1,2,3,4,5,6,7,8,9)
    val listExit = List(1,2,3,4,5,6,7,8,9)
    ListsScala.removeAt(list,10) shouldBe(listExit)
  }
  "insertAt con List(1,2,3,4,5,6,7,8,9), 50 y 5" should "List(1,2,3,4,50,5,6,7,8,9)" in {
    val list = List(1,2,3,4,5,6,7,8,9)
    val listExit = List(1,2,3,4,50,5,6,7,8,9)
    ListsScala.insertAt(list,50,5) shouldBe(listExit)
  }
  "range con 5 y 10" should "List(5,6,7,8,9,10)" in {
    val start = 5
    val end = 10
    val listExit = List(5,6,7,8,9,10)
    ListsScala.range(start,end) shouldBe(listExit)
  }

}
