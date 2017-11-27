package cracking

object Btree {
  def from(valuesToAdd: List[Int]): Btree = {
    val btree  = new Btree()
    valuesToAdd.foreach( v => btree.insert(v))
    btree
  }
}

class Btree(){
  var rootNode:Node = null

  def insert(valuesToAdd: List[Int]): Btree = {
    valuesToAdd.foreach(v => insert(v))
    return this
  }

  def insert(valueToAdd: Int): Btree = {
    if(rootNode==null){
      rootNode = new Node(valueToAdd)
    }else{
      rootNode.insert(valueToAdd)
    }
    return this
  }

  def has(valueToFind: Int): Boolean = {
    return rootNode.isPresent(valueToFind)
  }
}

class Node(var value: Int) {
  var nodeLeft: Node = null
  var nodeRight: Node = null

  def insert(valueToAdd: Int): Unit = {
    if (valueToAdd != value) {
      if (valueToAdd < value) {
        if (nodeLeft == null) {
          nodeLeft = new Node(valueToAdd)
        } else {
          nodeLeft.insert(valueToAdd)
        }
      } else {
        if (nodeRight == null) {
          nodeRight = new Node(valueToAdd)
        } else {
          nodeRight.insert(valueToAdd)
        }
      }
    }
  }

  def isPresent(valueToFind: Int): Boolean = {
    if (value.equals(valueToFind)) return true
    if (valueToFind < value && nodeLeft != null) return nodeLeft.isPresent(value)
    if (valueToFind > value && nodeRight != null) return nodeRight.isPresent(value)
    return false
  }
}