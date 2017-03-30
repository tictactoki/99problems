import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wong on 26/03/17.
  */
class ListSpec extends FlatSpec with Matchers {

  val list = List(1,4,5,6,7,8,10)
  val drop = List(1,4,5,6,7,8)
  val palindromeList = List(1,2,3,2,1)
  val duplicateList = List(1,1,4,4,5,5,6,6,7,7,8,8,10,10)
  val nestedList = List(List(1,4),5,List(6,7),8,List(10))
  val compressList = List(1,1,1,1,4,4,5,6,7,7,1,1,8,10)
  val encodeList = List((4,1),(2,4),(1,5),(1,6),(2,7),(2,1),(1,8),(1,10))
  val encodeModifiedList = List((4,1),(2,4),5,6,(2,7),(2,1),8,10)
  val packList = List(List(1,1,1,1), List(4,4), List(5), List(6), List(7,7), List(1,1), List(8), List(10))

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

  "A pack function" should "create a list with duplicate value" in {
    ListFunction.pack(compressList) should equal((packList))
  }

  "An encode function" should "return list with tuple contains number of duplicate value" in {
    ListFunction.encode(compressList) should equal(encodeList)
    ListFunction.encode2(compressList) should equal(encodeList)
  }

  "An encode modified function" should "modified encoded result" in {
    ListFunction.encoreModified(compressList)
  }

  "A decode function" should "decode an encoded list" in {
    ListFunction.decode(encodeList) should equal(compressList)
  }

  "A duplicate function" should "duplicate value in list" in {
    ListFunction.duplicate(list) should equal(duplicateList)
    ListFunction.duplicate2(list) should equal(duplicateList)
    ListFunction.duplicateN(list,2) should equal(duplicateList)
    ListFunction.duplicateN(list,3) should not equal(duplicateList)
  }

  "A drop function" should "drop value in list at n indice" in {
    ListFunction.drop(list, 6) should equal (drop)
  }

  "A split function" should "split the list" in {
    ListFunction.split(list,1) should equal((List(1), List(4,5,6,7,8,10)))
  }

  "A slice function" should "slice the list" in {
    ListFunction.slide(1,3,list) should equal(List(4,5,6))
  }

}
