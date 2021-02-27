package funciones

object Recursion {
  //Esta función recibe un número entero y devuelve su factorial.
  def factorial(n:Int):Int = n match{
    case 0 => 1
    case 1 => 1
    case _ => n * factorial(n-1)
  }
  //Esta función recibe un número entero y devuelve su factorial usando recursividad de cola.
  @annotation.tailrec def factorialRC(n:Int,acum:Int):Int = n match{
    case 0 => acum
    case 1 => acum
    case _ => factorialRC(n-1,acum * n)
  }
  //Esta función devuelve los n primeros números de la función fibonacci
  def f(n:Int):Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => f(n - 1) + f(n - 2)
  }
}
