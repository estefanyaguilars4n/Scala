package funciones

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuncionesSpec extends AnyFlatSpec with Matchers {

  "" should "n" in{
    val devengado = 500000
    val deducciones = 250000
    Funciones.compSalario(Funciones.calSalario,devengado,deducciones) shouldBe(250000)
  }
}
