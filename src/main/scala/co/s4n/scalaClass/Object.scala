package co.s4n.scalaClass
/** Ejercicio 1. */
object comp {
  private def cuadrado(x:Float):Float = {
    x * x
  }
  def cubo(y:Float):Float = {
    y * cuadrado(y)
  }
}
/** Ejercicio 2. */
object comp2 {
  private def cuadrado(x:Long):Long = {
    x * x
  }
  def cubo(y:Long):Long = {
    y * cuadrado(y)
  }
}
/** Ejercicio 3. */
object prueba {
  def x = {
    println("x")
    1
  }
  val y = {
    println("y")
    x + 2
  }
  def z = {
    println("z")
    x
    x + "c"
  }
}
/** Ejercicio 6. */
class Conductor(val nombre:String,val apellido:String,val totalCarreras:Int,val carrerasTerminadas:Int) {
  def carrerasNoTerminadas = totalCarreras - carrerasTerminadas
}
class Escuderia(val nombre:String,val conductor:Conductor) {}
/** Ejercicio 7.
class Contador(val x:Int){
  def incr() = new Contador(x + 1)
  def decr() = new Contador(x - 1)
}*/
/** Ejercicio 8.
class Contador(val valor:Int,val modificacion:Int){
  def this(valor:Int) = this(valor, 1)
  def incr() = new Contador(valor + modificacion,modificacion)
  def decr() = new Contador(valor - modificacion,modificacion)
}*/
/** Ejercicio 9. */
class Contador(val valor:Int,val modificacion:Int){
  def this(valor:Int) = this(valor, 1)
  def incr() = new Contador(valor + modificacion,modificacion)
  def decr() = new Contador(valor - modificacion,modificacion)
  def ajuste(sumador:Sumador) = new Contador(sumador.adicionar(valor))
}
class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}