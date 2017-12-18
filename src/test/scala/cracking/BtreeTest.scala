package cracking

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable

class BtreeTest extends FlatSpec with Matchers {

  "Create a node with value 1" should "insert in first value" in {
    var node = new Node(1)

    node.value should be (1)
  }

  "Insert 2,3,1" should "insert 1 in left and 3 in right" in {
    var btree = new Btree()
    btree insert 2 insert 3 insert 1
    btree.rootNode.value should be (2)
    btree.rootNode.nodeLeft.get.value should be (1)
    btree.rootNode.nodeRight.get.value should be (3)
  }

  "isPresent 2 in 3,5,12" should "return false" in {
    var btree = Btree.from(List(3,5,12))
    btree has 2 should be (false)
  }

  "isPresent 3 in 3,5,12" should "return true" in {
    var btree = Btree.from(List(3,5,12))
    btree has 3 should be (true)
  }

  "Print 2,3,1" should "insert 1 in left and 3 in right" in {
    var btree = new Btree()
    btree insert(List(7,5,0,1,2,3,4,6,13,12,11,10,9,8,14,19,17,15,16,18))
    println(btree printByLine false)
    println(btree printByLine true)
  }

}
