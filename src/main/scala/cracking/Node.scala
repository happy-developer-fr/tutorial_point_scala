package cracking

class Node(val value:Int, var nodeLeft:Node, var nodeRight:Node){

  def insert(valueToAdd:Int): Unit ={
    if (valueToAdd<value){
      if(nodeLeft==null) {
        nodeLeft = new Node(valueToAdd,null,null)
      }else{
        nodeLeft.insert(valueToAdd)
      }
    }else{
      if(nodeRight==null) {
        nodeRight = new Node(valueToAdd,null,null)
      }else{
        nodeRight.insert(valueToAdd)
      }
    }
  }
}

