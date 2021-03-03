package co.s4n.scalaClass
/** Ejercicio 4. */
class Gato(val nombre:String, val color:String, val comida:String) {
  def info = s"$nombre $color $comida"
}
/** Ejercicio 5. */
object ventaDeChurrus {
  def despachar(gato:Gato):Boolean =  if (gato.comida == "Churrus") true else false
}
