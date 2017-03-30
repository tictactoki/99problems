import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by stephane on 28/03/2017.
  */
class TreeSpec extends FlatSpec with Matchers {

  val symetric = Node(4, Node(3), Node(5))



  "A symmetric function" should "return true" in {
    symetric.isSymmetric should equal (true)
  }

  "Addvalue function" should "add an element in binary tree" in {
    val node = Node(6,Node(5),Node(7))
    val res = Node(6,Node(5,Node(4), End), Node(7))
    node.addValue(4) should equal(res)
  }





}
