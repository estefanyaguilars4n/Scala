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
/** Ejercicio 7. */
class Contador1(val x:Int){
  def incr() = new Contador(x + 1)
  def decr() = new Contador(x - 1)
}
/** Ejercicio 8. */
class Contador2(val valor:Int,val modificacion:Int){
  def this(valor:Int) = this(valor, 1)
  def incr() = new Contador(valor + modificacion,modificacion)
  def decr() = new Contador(valor - modificacion,modificacion)
}
/** Ejercicio 9. */
class Contador(val valor:Int,val modificacion:Int){
  def this(valor:Int) = this(valor, 1)
  def incr() = new Contador(valor + modificacion,modificacion)
  def decr() = new Contador(valor - modificacion,modificacion)
  def ajuste(sumador:Sumador) = new Contador(sumador.adicionar(valor))
}
/**
 *
 * @param monto
 */
class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}
/** Ejercicio 10. */
class Persona(val nombre:String,val apellido:String){
  def nombreCompleto:String = s"$nombre $apellido"
}
object Persona{
  def apply(nombreCompleto: String):Persona =
    new Persona(nombreCompleto.split(" ")(0),nombreCompleto.split(" ")(1))
}
/** Ejericio 11. */
class Director(
              val nombre:String,
              val apellido:String,
              val nacimiento:Int
              ) {
  def nombreCompleto:String = s"$nombre $apellido"
  def copy(nombre:String = this.nombre,
           apellido:String = this.apellido,
           nacimiento:Int = nacimiento):Director =
    new Director(nombre,apellido,nacimiento)
}
object Director{
  def apply(nombre: String, apellido: String, nacimiento: Int): Director = new Director(nombre, apellido, nacimiento)
  def esMayor(director1:Director,director2:Director):Director =
    if (director1.nacimiento < director2.nacimiento) director1 else director2
}
class Pelicula(
              val nombre:String,
              val presentacion:Int,
              val rangoIMDB:Double,
              val director:Director
              ) {
  def directorEdad = presentacion - director.nacimiento
  def esDirigidaPor(director:Director) = this.director == director
  def copy(nombre:String = this.nombre,
           presentacion:Int = this.presentacion,
           rangoIMDB:Double = this.rangoIMDB,
           director:Director = this.director
          ):Pelicula = new Pelicula(nombre,presentacion,rangoIMDB,director)
}
object Pelicula{
  def apply(nombre: String, presentacion: Int, rangoIMDB: Double, director: Director): Pelicula =
    new Pelicula(nombre, presentacion, rangoIMDB, director)
  def mejorCalificada(pelicula1:Pelicula,pelicula2:Pelicula):Pelicula =
    if (pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1 else pelicula2
  def mayorDirectorEnElTiempo(pelicula1:Pelicula,pelicula2:Pelicula):Director =
    if (pelicula1.directorEdad > pelicula2.directorEdad) pelicula1.director else pelicula2.director
}