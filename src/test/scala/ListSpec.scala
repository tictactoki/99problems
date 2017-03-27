import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wong on 26/03/17.
  */
class ListSpec extends FlatSpec with Matchers {

  val list = List(1,4,5,6,7,8,10)
  val palindromeList = List(1,2,3,2,1)
  val nestedList = List(List(1,4),5,List(6,7),8,List(10))
  val compressList = List(1,1,1,1,4,4,5,6,7,7,1,1,8,10)

  "A last function" should "return the last element of the list" in {
    ListFunction.last(list) should equal(10)
  }

  "A last function" should "return the an exception if the list is Nil" in {
    a[NoSuchElementException] should be thrownBy {
      ListFunction.last(Nil)
    }
  }

  "A penultimate function" should "return the last but one element" in {
    ListFunction.penultimate(list) should equal(8)
  }

  "A nth function" should "return the k th element" in {
    ListFunction.nth(2,list) should equal(5)
  }

  "A length function" should "return the size of the list" in {
    ListFunction.length(list) should equal(7)
  }

  "A reverse function" should "reverse the list" in {
    ListFunction.reverse(list) should equal (list.reverse)
  }

  "An isPalindrome function" should "return true if the list is a palindrome" in {
    ListFunction.isPalindrome(palindromeList) should be(true)
  }

  "A flatten function" should "flat the list" in {
    ListFunction.flatten(nestedList) should equal(list)
  }

  "A compress function" should "delete duplicate value on list" in {
    ListFunction.compress(compressList) should equal(list)
  }

}
