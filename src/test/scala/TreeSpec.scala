import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by stephane on 28/03/2017.
  */
class TreeSpec extends FlatSpec with Matchers {

  val symmetric = Node(5, Node(3, Node(2), Node(4)), Node(7, Node(6), Node(8)))

  "A symmetric function" should "return true" in {
    symmetric.isSymmetric should equal (true)
  }

  "An addValue function" should "add an element in binary tree" in {
    val node = Node(6,Node(5),Node(7))
    val res = Node(6,Node(5,Node(4), End), Node(7))
    node.addValue(4) should equal(res)
  }

  println(Tree.symmetricBalancedTrees(5,"x"))

  "A symmetricBalancedTrees function" should "return a list of balanced and symmetric trees" in {
    Tree.symmetricBalancedTrees(5,"x").length should equal(4)
  }




}
