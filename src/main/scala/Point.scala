package scala

object PointRun{
  def main(args:Array[String]) {
    var l = new Location(1, 2,3)
    l.move(1, 1,1)
  }
}

class Point(val xc: Int, val yc: Int) {
  var x = xc
  var y = yc

  def move(dx: Int, dy: Int) {
    x = x + dx
    y = y + dy
    println("Point x location = " + x)
    println("Point y location = " + y)
  }
}

class Location(override val xc: Int, override  val yc:Int, val zc:Int) extends Point(xc,yc){
  var z = zc
  def move(dx:Int, dy:Int, dz:Int): Unit ={
    super.move(dx,dy)
    z = z + dz
    println("Point z location = " + z)
  }
}


