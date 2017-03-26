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


}

