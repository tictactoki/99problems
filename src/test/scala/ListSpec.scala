import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wong on 26/03/17.
  */
class ListSpec extends FlatSpec with Matchers {

  val list = List(1,4,5,6,7,8,10)

  "A last function" should "return the last element of the list" in {
    ListFunction.last(list) should equal(10)
  }

  "A last function" should "return the an exception if the list is Nil" in {
    a[NoSuchElementException] should be thrownBy {
      ListFunction.last(Nil)
    }
  }

  "Penultimate function" should "return the last but one element" in {
    ListFunction.penultimate(list) should equal(8)
  }

  "nth function" should "return the k th element" in {
    ListFunction.nth(2,list) should equal(5)
  }

  "length function" should "return the size of the list" in {
    ListFunction.length(list) should equal(7)
  }

}
