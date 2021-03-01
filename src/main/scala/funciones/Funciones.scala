package funciones

case object Funciones {
  //Ejercicio 1. Función literal que recibe los dos lados rectos de un triángulo y devuelve el área de dicho triángulo
  val areaTrianguloRectangulo = (a:Int, b:Int) => { (a * b) / 2 }

  //Ejercicio 2. Función literal que recibe el radio de un circulo y devuelve el área de dicho circulo
  val areaDeUnCirculo = new Function1[Int,Double] {def apply(r:Int):Double = { scala.math.Pi * scala.math.pow(r,2)}}

  //Ejercicio 3. Función literal que recibe el devengado de un trabajador y sus deducciones y devuelve el salario a
  // pagar
  val calSalario = (devengado:Double, deducciones:Double) => { devengado - deducciones }

  //Ejercicio 4. Función que recibe el devengado de un trabajador y sus deducciones y devuelve el salario a pagar con un
  // bono del 10%
  val calSalarioBono = (devengado:Double, deducciones:Double) => { devengado * 1.1 - deducciones }

  //Ejercicio 5. Función que recibe una función de operación de salario, el devengado y las deducciones de un empleado y
  // devuelve el resultado de dicho función aplicada al devengado y las deducciones introducidas
  def compSalario(f:(Double,Double)=>Double, devengado:Double, deducciones:Double) = f(devengado,deducciones)

  //Ejercicio 6. Función que recibe un double con el porcentaje de un bono para aplicar directamente y devuelve una
  // función que recibe el devengado y las deducciones de un empleado para devolver el salario con el bono aplicado
  def genCalSalarioBono(bono:Double):(Double, Double) => Double = {
    (devengado:Double,deducciones:Double) => { devengado * bono - deducciones }}

  //Ejercicio 7. Función literal que aplica 5% de bono al recibir el devengado y las deducciones
  val calSalario5 = genCalSalarioBono(1.05)

  //Ejercicio 8. Función literal que aplica 20% de bono al recibir el devengado y las deducciones
  val calSalario20 = genCalSalarioBono(1.2)

  ////Ejercicio 10. Función literal que aplica 5% de bono al recibir el devengado y las deduccionesFunción que recibe
  // el devengado y las deducciones de un empleado y usando un valor de bono declarado fuera de ésta calcula el salario
  // con el bono
  val bonoClausura = 1.15
  def calSalarioBonoClausura(devengado:Double, deducciones:Double) = {devengado * bonoClausura - deducciones}

  //Ejercicio 11. Función que desde la aplicación parcial de la función genCalSalarioBono contiene un valor de
  // bono específico de 15% para usar calculando el salario de un empleado
  def calSalario15():(Double, Double)=>Double = {
    genCalSalarioBono(1.15)
  }

  //Ejercicio 13.
  def genCalSalarioBono2(devengado:Double, deduccion:Double):Double => Double = {
    (bono:Double) => devengado * bono - deduccion
  }


}


