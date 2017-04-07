import scala.util.Random

/**
  * Created by stephane on 28/03/2017.
  */
sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](value: U): Tree[U]
  def countLeaf: Int
  def leafList: List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def isSymmetric: Boolean = {
    def size(n: Int, tree: Tree[T]): Int = tree match {
      case Node(v,l,r) => {
        return (size(n+1,l) + size(n+1, r))
      }
      case End => {
        return n
      }

    }
    return (size(0,left) == size(0,right))
  }

  override def addValue[U >: T <% Ordered[U]](value: U): Tree[U] = {
    if(value < this.value) Node(this.value, left.addValue(value),right)
    else Node(this.value, left, right.addValue(value))
  }

  override def countLeaf: Int = (left, right) match {
    case (End,End) => 1
    case (_,_) => left.countLeaf + right.countLeaf
  }

  override def leafList: List[T] = (left, right) match {
    case (End,End) => List(value)
    case (_,_) => left.leafList ::: right.leafList
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isSymmetric: Boolean = true

  override def addValue[U >: Nothing <% Ordered[U]](value: U): Tree[U] = Node(value)

  override def countLeaf: Int = 0

  override def leafList: List[Nothing] = Nil
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {


  def cBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case n if n < 1 => List(End)
    case n if (n % 2 == 1) => {
      val subTrees = cBalanced(n / 2, value)
      subTrees.flatMap { t => subTrees.map { r => Node(value, t, r) } }
    }
    case n if (n % 2 == 0) => {
      val lt = cBalanced((n-1)/2,value)
      val gt = cBalanced((n-1)/2+1, value)
      lt.flatMap(l => gt.flatMap(r => List(Node(value,l,r), Node(value,r,l))))
    }
  }

  def symmetricBalancedTrees[T](n: Int, value: T): List[Tree[T]] = cBalanced(n,value).filter(_.isSymmetric)



}