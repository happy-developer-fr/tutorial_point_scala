package cracking

import scala.collection.mutable
import scala.collection.mutable.{MutableList, _}

object Btree {
  def from(valuesToAdd: List[Int]): Btree = {
    val btree = new Btree()
    valuesToAdd.foreach(v => btree.insert(v))
    btree
  }
}

class Btree() {
  var rootNode: Node = null

  def printByLine(godMod: Boolean): String = {
    rootNode.printByLine(godMod)
  }

  def printBySpaces(godMod: Boolean): String = {
    rootNode.printBySpaces(godMod)
  }

  def printByConcat(godMod: Boolean): String = {
    rootNode.printByConcat(godMod)
  }

  def insert(valuesToAdd: List[Int]): Btree = {
    valuesToAdd.foreach(v => insert(v))
    return this
  }

  def insert(valueToAdd: Int): Btree = {
    if (rootNode == null) {
      rootNode = new Node(valueToAdd)
    } else {
      rootNode.insert(valueToAdd)
    }
    return this
  }

  def has(valueToFind: Int): Boolean = {
    return rootNode.isPresent(valueToFind)
  }

}
case class Spaces(var l:Int, var r:Int)

class Node(val value: Int) {
  var leftMarginBalanced = 0

  var leftOffset, rightOffset = 0
  var leftMargin = -1
  var nodeLeft: Option[Node] = None
  var nodeRight: Option[Node] = None

  var spaces:Spaces = Spaces(0,0)

  def computeOffsets(_leftMargin: Int): Unit = {
    if (nodeLeft.isDefined) {
      nodeLeft.get.computeOffsets(_leftMargin)
      leftOffset = nodeLeft.get.leftOffset + nodeLeft.get.rightOffset + 1
    }

    this.leftMargin = this.leftOffset + _leftMargin

    if (nodeRight.isDefined) {
      nodeRight.get.computeOffsets(this.leftMargin + 1)
      rightOffset = nodeRight.get.leftOffset + nodeRight.get.rightOffset + 1
    }

    if(nodeLeft.isDefined && nodeRight.isDefined){
      this.leftMarginBalanced = nodeLeft.get.leftMarginBalanced + math.max(this.leftMargin - nodeLeft.get.leftMargin, nodeRight.get.leftMargin - this.leftMargin)
    }else if(nodeLeft isDefined){
      this.leftMarginBalanced = nodeLeft.get.leftMarginBalanced + (leftMargin - nodeLeft.get.leftMargin)
    }else if(nodeRight isDefined){
      this.leftMarginBalanced = nodeRight.get.leftMarginBalanced + (leftMargin - nodeRight.get.leftMargin)
    }else{
      this.leftMarginBalanced = leftMargin
    }
  }




  def insert(valueToAdd: Int): Unit = {
    if (valueToAdd != value) {
      if (valueToAdd < value) {
        if (nodeLeft.isEmpty) {
          nodeLeft = Some(new Node(valueToAdd))
        } else {
          nodeLeft.get.insert(valueToAdd)
        }
      } else {
        if (nodeRight.isEmpty) {
          nodeRight = Some(new Node(valueToAdd))
        } else {
          nodeRight.get.insert(valueToAdd)
        }
      }
    }
  }

  def isPresent(valueToFind: Int): Boolean = {
    if (value.equals(valueToFind)) return true
    if (valueToFind < value && nodeLeft.isDefined) return nodeLeft.get.isPresent(value)
    if (valueToFind > value && nodeRight.isDefined) return nodeRight.get.isPresent(value)
    return false
  }


  def dispatchByLine(lineLevel: Int, matrix: mutable.MutableList[mutable.MutableList[Node]]): mutable.MutableList[mutable.MutableList[Node]] = {
    def defaultLine() = {
      var lineMatrixTemp: MutableList[Node] = mutable.MutableList()
      matrix += lineMatrixTemp
      lineMatrixTemp
    }

    var lineMatrix: MutableList[Node] = matrix.get(lineLevel).getOrElse(defaultLine())
    lineMatrix += this
    nodeLeft.map(nl => nl.dispatchByLine(lineLevel + 1, matrix))
    nodeRight.map(nr => nr.dispatchByLine(lineLevel + 1, matrix))
    matrix
  }

  def printByLine(godMod: Boolean): String = {
    var result = ""
    this.computeOffsets(0)
    var matrix = dispatchByLine(0, mutable.MutableList())
    for (lm <- matrix) {
      var margeToSupress = 0
      for (n <- lm) {
        result += getSpaces(n.leftMarginBalanced - margeToSupress) + n.value
        if (godMod) {
          result += ": [" + n.leftMarginBalanced + "|" + n.leftOffset + "/" + n.rightOffset + "]"
        }
        margeToSupress = n.leftMarginBalanced + 1
      }

      result += "\n"
    }
    result
  }

  def computeSpaces(spacesInit:Spaces): Spaces ={
    if(nodeLeft isEmpty){
      spaces.l = spacesInit.l +1
    }else{
      nodeLeft.get.computeSpaces(Spaces(spacesInit.l+1,spacesInit.r+1))
      spaces.l = nodeLeft.get.spaces.l + nodeLeft.get.spaces.r - spacesInit.l
    }

    if(nodeRight isEmpty){
      spaces.r = spacesInit.r +1
    }else{
      nodeRight.get.computeSpaces(Spaces(spacesInit.l+1,spacesInit.r+1))
      spaces.r = nodeRight.get.spaces.l + nodeRight.get.spaces.r - spacesInit.r
    }
    spaces
  }

  def printBySpaces(godMod: Boolean): String = {
    var result = ""
    this.computeSpaces(Spaces(0,0))
    var matrix = dispatchByLine(0, mutable.MutableList())
    for (lm <- matrix) {
      for (n <- lm) {
        result += getSpaces(n.spaces.l) + n.value + getSpaces(n.spaces.r)
        if (godMod) {
          result += ": [" + n.spaces.l + "/" + n.spaces.r + "]"
        }
      }
      result += "\n"
    }
    result
  }

  def printByConcat(godMod: Boolean): String = {
    ""
  }

  private def getSpaces(spacesCount: Int): String = {
    var spacesToPrint = ""
    for (spaces <- 0 until spacesCount) {
      spacesToPrint += " "
    }
    spacesToPrint
  }
}