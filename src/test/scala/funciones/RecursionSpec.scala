package funciones

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RecursionSpec extends AnyFlatSpec with Matchers {

  "La funcion factorial al recibir el entero 5" should "debería devolver su factorial 120" in {
    val n = 5
    Recursion.factorial(5) shouldBe(120)
  }
  "La función factorial al recibir el entero 8" should "debería devolver su factorial 40320" in {
    val n = 8
    Recursion.factorial(8) shouldBe(40320)
  }
  "La función factorialRC al recibir el entero 10" should "debería devolver su factorial 3628800" in {
    val n = 10
    Recursion.factorialRC(n) shouldBe(3628800)
  }
  "La función f enviando la posición 7" should "debería devolver el número 13" in {
    val n = 7
    Recursion.f(n) shouldBe(13)
  }
  "La función f enviando la posición 14" should "debería devolver el número 377" in {
    val n = 14
    Recursion.f(n) shouldBe(377)
  }
}
