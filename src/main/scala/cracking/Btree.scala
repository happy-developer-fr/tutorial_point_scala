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

  def printByLine():String = {
    rootNode.printByLine()
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

  def print(): String = {
    return rootNode print
  }
}

class Node(var value: Int) {
  var nodeLeft: Option[Node] = None
  var nodeRight: Option[Node] = None

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
    if (valueToFind < value && nodeLeft != null) return nodeLeft.get.isPresent(value)
    if (valueToFind > value && nodeRight != null) return nodeRight.get.isPresent(value)
    return false
  }

  def countLeft(): Int = {
    if (nodeLeft.isDefined) {
      nodeLeft.get.countLeft() + 1
    } else {
      1
    }
  }

  def dispatchByLine(lineLevel:Int, matrix: mutable.MutableList[mutable.MutableList[Node]]):mutable.MutableList[mutable.MutableList[Node]] ={
    def defaultLine() = {
      var lineMatrixTemp : MutableList[Node] = mutable.MutableList()
      matrix += lineMatrixTemp
      lineMatrixTemp
    }

    var lineMatrix : MutableList[Node] = matrix.get(lineLevel).getOrElse(defaultLine())
    lineMatrix += this
    nodeLeft.map(nl => nl.dispatchByLine(lineLevel + 1, matrix))
    nodeRight.map(nr => nr.dispatchByLine(lineLevel + 1, matrix))
    matrix
  }

  def printByLine(): String = {
    var result = ""
    var matrix =dispatchByLine(0, mutable.MutableList())
    matrix.map(lm => lm.map(n=>n.value) mkString " ") mkString "\n"
  }

  def print(): String = {
    var treePrint: String = ""
    val leftCount: Int = countLeft()
    treePrint += getSpaces(leftCount)
    treePrint += value + "\n"
    if(nodeLeft.nonEmpty) {
      treePrint += getSpaces(leftCount) + "/ \\" + "\n"
      treePrint += nodeLeft.map(n => n print)
    }
    treePrint
  }

  private def getSpaces(spacesCount:Int):String={
    var spacesToPrint = ""
    for (spaces <- 0 to spacesCount) {
      spacesToPrint += " "
    }
    spacesToPrint
  }
}