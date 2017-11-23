package scala

/**
  * Created by jcottet on 22/11/17.
  */
object HelloWorld {

  def main(args: Array[String]) {
    println(
      """Hello,
        |world!\b\b"""" stripMargin) // prints Hello World
  }
}
