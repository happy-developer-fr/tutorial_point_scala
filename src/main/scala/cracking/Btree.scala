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

class Node(val value: Int) {
  var leftOffset, rightOffset: Int = 0
  var leftMargin: Int = -1
  var nodeLeft: Option[Node] = None
  var nodeRight: Option[Node] = None

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
        result += getSpaces(n.leftMargin - margeToSupress) + n.value
        if (godMod) {
          result += ": [" + n.leftMargin + "|" + n.leftOffset + "/" + n.rightOffset + "]"
        }
        margeToSupress = n.leftMargin + 1
      }

      result += "\n"
    }
    result
  }

  private def getSpaces(spacesCount: Int): String = {
    var spacesToPrint = ""
    for (spaces <- 0 until spacesCount) {
      spacesToPrint += " "
    }
    spacesToPrint
  }
}