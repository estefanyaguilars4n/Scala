package funciones

object Recursion {
  //Esta función recibe un número entero y devuelve su factorial.
  def factorial(n:Int):Int = n match{
    case 0 => 1
    case 1 => 1
    case _ => n * factorial(n - 1)
  }
}
