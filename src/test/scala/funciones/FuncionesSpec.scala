package funciones

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuncionesSpec extends AnyFlatSpec with Matchers {

  "La función areaTrianguloRectangulo enviando los valores 5 y 4" should "debería devolver área 10" in {
    val ladoA = 4
    val ladoB = 5
    Funciones.areaTrianguloRectangulo(ladoA,ladoB) shouldBe(10)
  }
  "la función areaDeUnCirculo enviando el radio " should "debería devolver el área " in {
    val radio = 2
    Funciones.areaDeUnCirculo(radio) shouldBe(12.566370614359172)
  }
  "La función calSalario al enviarle devengado de 500000 y deducciones de 250000" should "debería devolver 250000 de " +
    "salario" in {
    val devengado = 500000
    val deducciones = 250000
    Funciones.calSalario(devengado,deducciones) shouldBe(250000)
  }
  "La función calSalarioBono al enviarle devengado de 1000000 y deducciones de 370000" should "debería devolver 730000 " +
    "de salario" in {
    val devengado = 1000000
    val deducciones = 370000
    Funciones.calSalarioBono(devengado,deducciones) shouldBe(730000)
  }
  "La función compSalario enviando la función calSalario, devengado de 700000 y deducciones de 150000" should "debería " +
    "devolver un salario de 550000" in {
    val devengado = 700000
    val deducciones = 150000
    Funciones.compSalario(Funciones.calSalario,devengado,deducciones) shouldBe(550000)
  }
  "La función compSalario enviando la función calSalarioBono, devengado de 800000 y deducciones de 170000" should "debería " +
    "devolver salario de 710000" in {
    val devengado = 900000
    val deducciones = 150000
    Funciones.compSalario(Funciones.calSalarioBono,devengado,deducciones) shouldBe(840000)
  }
  "La función genCalSalarioBono con el valor 1.05" should "debería devolver una función que cuando reciba de devengado " +
    "2000000 y de deducciones 300000 devuelva 1800000 de salario" in {
    val bono = 1.05
    val devengado = 2000000
    val deducciones = 300000
    val funcion = Funciones.genCalSalarioBono(bono)
    funcion(devengado, deducciones) shouldBe(1800000)
  }
  "La función calSalario5 enviando devengado de 2000000 y deducciones de 300000" should "debería devolver salario de " +
    "180000" in {
    val devengado = 2000000
    val deducciones = 300000
    Funciones.calSalario5(devengado, deducciones) shouldBe(1800000)
  }
  "La función calSalario20 enviando devengado de 2000000 y deducciones de 300000" should "debería devolver salario de " +
    "2100000" in {
    val devengado = 2000000
    val deducciones = 300000
    Funciones.calSalario20(devengado, deducciones) shouldBe(2100000)
  }
  "La función calSalarioBonoClausura enviando devengado de 5000000 y deducciones de 635000 usando el valor de bono 1.15 " +
    "declarado fuera de la función" should "debería devolver 5115000 de salario" in {
    val devengado = 5000000
    val deducciones = 635000
    Funciones.calSalarioBonoClausura(devengado, deducciones) shouldBe(5115000)
  }
  "La función calSalario15" should "debería devolver una función que al enviarle 4000000 de devengado y 500000 de " +
    "deducciones devuelva 4100000 de salario" in {
    val devengado = 4000000
    val deducciones = 500000
    val funcion = Funciones.calSalario15()
    funcion(devengado, deducciones) shouldBe(4100000)
  }
  "La funcion genCalSalarioBono2 al enviarle devengado de 4700000 y dedudcciones de 600000" should "debería devolver " +
    "una función que reciba el porcentaje de bono 1.07 y devuelva salario de 4429000" in {
    val devengado = 4700000
    val deducciones = 600000
    val funcion = Funciones.genCalSalarioBono2(devengado, deducciones)
    funcion(1.07) shouldBe(4429000)
  }
  "La funcion genCalSalarioBono2 al enviarle devengado de 4700000 y dedudcciones de 600000" should "debería devolver " +
    "una función que reciba el porcentaje de bono 1.15 y devuelva salario de 4805000" in {
    val devengado = 4700000
    val deducciones = 600000
    val funcion = Funciones.genCalSalarioBono2(devengado, deducciones)
    funcion(1.15) shouldBe(4805000)
  }
}
