import scala.annotation.tailrec

/**
  * Created by wong on 26/03/17.
  */
object ListFunction {

  def last(list: List[Int]): Int = list match {
    case h :: Nil => h
    case h :: t => last(t)
    case Nil => throw new NoSuchElementException
  }

  def penultimate(list: List[Int]): Int = list match {
    case l :: h :: Nil => l
    case l :: Nil => l
    case h :: t => penultimate(t)
    case Nil => throw new NoSuchElementException
  }

  def nth(n: Int, list: List[Int]): Int = (n,list) match {
    case (0,h::_) => h
    case (n,h::t) => nth(n-1,t)
    case (n,Nil) => throw new NoSuchElementException
  }

  def length(list: List[Int]): Int = {

    def length(n: Int, list: List[Int]): Int = list match {
      case Nil => n
      case h::t => length(n+1,t)
    }
    length(0,list)
  }

  def reverse(list: List[Int]) = list.foldLeft(List[Int]())((acc,list) => list :: acc)

  def sum(list: List[Int]) = list.foldLeft(0)((acc,value) => acc + value)

  def isPalindrome(list: List[Int]) = list == list.foldLeft(List[Int]())((acc,list) => list :: acc)

  def flatten(nested: List[Any]): List[Any] = nested.flatMap {
    case l: List[_] => flatten(l)
    case any => List(any)
  }

  def compress[T](list: List[T]) = {

    @tailrec
    def compress(list: List[T], aux: List[T]): List[T] = list match {
      case Nil => aux
      case h :: t => if(!aux.contains(h)) compress(t,aux:::List(h)) else compress(t,aux)
    }

    compress(list,Nil)
  }

  

}

