package co.s4n.counters

class Counter(val value:Int, val step:Int) {
  def doStep():Counter = copy(value + step)
  def doStep(step:Int):Counter = new Counter(value + step,step)
  override def toString():String = s"value: $value step: $step"
  def copy(value:Int = this.value, step:Int = this.step):Counter =
    new Counter (value,step)
}
object Counter {
  def apply(value:Int,step:Int) = new Counter(value,step)
  def main(args:Array[String]):Unit = {
    val c:Counter = Counter(1,1)
    val c2:Counter = Counter(2,2)
    println(c.doStep(1).doStep(2).doStep(3))
    println(c2.doStep(1).doStep(2).doStep(3))
  }
}