package co.s4n.traitScala

sealed trait Felino {
  def color:String
  def sonido:String
}

/** Clase León que extiende del trait Felinos
 *
 * @param color
 * @param sonido
 * @param tamañoMelena
 */
case class Leon(override val color:String,override val sonido:String,val tamañoMelena:Int) extends Felino

/** Clase Tigre que extiende del trait Felinos
 *
 * @param color
 * @param sonido
 */
case class Tigre(override val color:String,override val sonido:String) extends Felino

/** Clase Jaguar que extiende del trait Felinos
 *
 * @param color
 * @param sonido
 */
case class Jaguar(override val color:String,override val sonido:String) extends Felino

/** Clase Gato que extiende del trait Felinos
 *
 * @param color
 * @param sonido
 * @param comida
 */
case class Gato(override val color:String,override val sonido:String,val comida:String) extends Felino

sealed trait Forma {
  def tamaño():Double
  def perimetro():Double
  def area():Double
}

/**
 *
 * @param radio
 */
case class Circulo(val radio:Double) extends Forma{
  override def tamaño():Double = this.radio * 2
  override def perimetro():Double = 2 * math.Pi * this.radio
  override def area():Double = math.Pi * radio * this.radio
  override def toString:String = s"Un circulo de radio $radio cm"
}

sealed trait Rectangular extends Forma {
  override def tamaño():Double = 4
  override def perimetro():Double
  override def area():Double
}

/**
 *
 * @param base
 * @param altura
 */
case class Rectangulo(val base:Double, val altura:Double) extends Rectangular{
  override def perimetro():Double = 2 * (this.base + this.altura)
  override def area():Double = this.base * this.altura
  override def toString:String = s"Un rectángulo de ancho $base cm y largo $altura cm"
}

/**
 *
 * @param lado
 */
case class Cuadrado(val lado:Double) extends Rectangular{
  override def perimetro():Double = this.lado * 4
  override def area():Double = lado * 2
  override def toString:String = s"Un cuadrado con lados de $lado cm"
}
object Draw {
  def apply(forma:Forma):Forma = forma match {
    case Circulo(radio)          => forma
    case Rectangulo(base,altura) => forma
    case Cuadrado(lado)          => forma
  }
}
case class Color(val red:Int,val green:Int,val blue:Int) {}
object Rojo extends Color (255,0,0)
object Amarillo extends Color (255,255,0)
object Rosa extends Color(247,191,190)
