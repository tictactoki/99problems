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

  def nth(n: Int, list: List[Int]): Int = (n, list) match {
    case (0, h :: _) => h
    case (n, h :: t) => nth(n - 1, t)
    case (n, Nil) => throw new NoSuchElementException
  }

  def length(list: List[Int]): Int = {

    def length(n: Int, list: List[Int]): Int = list match {
      case Nil => n
      case h :: t => length(n + 1, t)
    }
    length(0, list)
  }

  def reverse(list: List[Int]) = list.foldLeft(List[Int]())((acc, list) => list :: acc)

  def sum(list: List[Int]) = list.foldLeft(0)((acc, value) => acc + value)

  def isPalindrome(list: List[Int]) = list == list.foldLeft(List[Int]())((acc, list) => list :: acc)

  def flatten(nested: List[Any]): List[Any] = nested.flatMap {
    case l: List[_] => flatten(l)
    case any => List(any)
  }

  def compress[T](list: List[T]) = {

    @tailrec
    def compress(list: List[T], aux: List[T]): List[T] = list match {
      case Nil => aux
      case h :: t => if (!aux.contains(h)) compress(t, aux ::: List(h)) else compress(t, aux)
    }
    compress(list, Nil)
  }

  def pack[T >: Null](list: List[T]): List[List[T]] = {

    @tailrec
    def packed(list: List[T], t: T = null, acc: List[T] = Nil, aux: List[List[T]] = Nil): List[List[T]] = list match {
      case Nil => aux ::: List(acc)
      case h :: tail => {
        if (h == t || t == null) packed(tail, h, h :: acc, aux)
        else {
          packed(tail, h, List(h), aux ::: List(acc))
        }
      }
    }
    packed(list)
  }

  def encode[T >: Null](list: List[T]) = {
    def encode(list: List[T], prev: T = null, n: Int = 0, aux: List[(Int, T)] = Nil): List[(Int, T)] = list match {
      case Nil => aux ::: List((n, prev) )
      case h :: tail =>
        if (h == prev || prev== null) encode(tail, h, n + 1, aux)
        else encode(tail, h, 1, aux ::: List((n, prev) ))
    }
    encode(list)
  }

  def encode2[T >: Null](list: List[T]): List[(Int, T)] = pack(list).map { list => (list.size, list.head) }

  def encoreModified[T >: Null](list: List[T]): List[Any] = {
    pack(list).map { list => if(list.size == 1) list.head else (list.size,list.head)}
  }

  def decode[T](list: List[(Int,T)]): List[T] = list.flatMap { case (n,t) => List.fill(n)(t) }

  def duplicate[T](list: List[T]) = {
    def duplicate(list: List[T], aux: List[T]): List[T] = list match {
      case Nil => aux
      case h :: tail => duplicate(tail, aux ::: List(h,h))
    }
    duplicate(list,Nil)
  }

  def duplicate2[T](list: List[T]) = list.flatMap { e => List(e,e)}

  def duplicateN[T](list: List[T], n: Int) = list.flatMap { e => List.fill(n)(e) }

}

