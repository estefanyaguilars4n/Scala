package funciones

object Recursion {
  //Esta función recibe un número entero y devuelve su factorial.
  def factorial(n:Int):Int = n match{
    case 0 => 1
    case 1 => 1
    case _ => n * factorial(n-1)
  }
  //Esta función recibe un número entero y devuelve su factorial usando recursividad de cola.
  def factorialRC(n:Int):Int = {
    @annotation.tailrec def factorialRCp(n:Int,acum:Int):Int = n match{
      case 0 => acum
      case 1 => acum
      case _ => factorialRCp(n-1,acum * n)
    }
    factorialRCp(n,1)
  }

  //Esta función devuelve el n número de la función fibonacci
  def f(n:Int):Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => f(n - 1) + f(n - 2)
  }
}
