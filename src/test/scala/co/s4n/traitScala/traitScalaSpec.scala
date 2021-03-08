package co.s4n.traitScala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class traitScalaSpec extends AnyFlatSpec with Matchers{
  "El método apply del objeto Draw enviándole la forma Circulo(12)" should "debería devolver el string Un circulo " +
    "de radio 12.0 cm" in {
    val circulo = Circulo(12)
    Draw(circulo).toString shouldBe("Un circulo de radio 12.0 cm")
  }
  "El método apply del objeto Draw enviándole la forma Rectangulo(15,10)" should "debería devolver el string " +
    "Un rectángulo de ancho 15.0 cm y largo 10.0 cm" in {
    val rectangulo = Rectangulo(15,10)
    Draw(rectangulo).toString shouldBe("Un rectángulo de ancho 15.0 cm y largo 10.0 cm")
  }
  "El método apply del objeto Draw enviándole la forma Cuadrado(16)" should "debería devolver el string " +
    "Un cuadrado con lados de 16.0 cm" in {
    val cuadrado = Cuadrado(16)
    Draw(cuadrado).toString shouldBe("Un cuadrado con lados de 16.0 cm")
  }
}
