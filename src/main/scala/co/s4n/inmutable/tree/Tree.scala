package co.s4n.inmutable.tree

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
  /*
  def depth[A](tree:Tree[A]):Int = {
    def depthp[A](tree:Tree[A],aux1:Int,aux2:Int):Int = tree match {
      case Leaf(_)      => aux
      case Branch(l, r) => depthp(l,aux1+1,aux2)  depthp(r,aux1,aux2+1)
    }
  }*/
}
