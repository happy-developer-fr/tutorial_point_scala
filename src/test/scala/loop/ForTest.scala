package loop

import org.scalatest.{FlatSpec, Matchers}

class ForTest extends FlatSpec with Matchers{

  "Double for" should "loop over two lists" in {
    var resultA_plus_B = 0
    var resultB = 0
    for(a <- 1 to 3; b <- 4 to 7){
      resultA_plus_B = a + b
      resultB = b
    }

    resultA_plus_B should be (10)
    resultB should be (7)
  }

  "Double sum 1-> 2 && 4 " should "sum 1*4 + sum 2*4" in {
    var resultSum = 0
    for(a <- 1 to 2; b <- 4 to 4){
      resultSum = resultSum + (a*b)
    }

    resultSum should be (12)
  }

  "Double sum 1-> 2 && 3 -> 4" should "sum 1*3 (3) + sum 2*3 (6) + 1*4 (4) + 2*4 (8)" in {
    var resultSum = 0
    for(a <- 1 to 2; b <- 3 to 4){
      resultSum = resultSum + (a*b)
    }

    resultSum should be (21)
  }

  "Double sum a = 1-> 2 && b = a -> 4" should "sum 1*1 (1) + sum 1*2 (2) + 2*2 (4)" in {
    var resultSum = 0
    for(a <- 1 to 2; b <- a to 2){
      resultSum = resultSum + (a*b)
    }

    resultSum should be (7)
  }

}
