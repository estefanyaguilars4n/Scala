package co.s4n.editorial

class Libro(val titulo:String,val autor:String,val ref:String) {
  def this(titulo:String,autor:String) = this(titulo,autor,"Ficción")
  def nombre = s"$titulo $autor $ref"
}
