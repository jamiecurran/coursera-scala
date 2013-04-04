package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def factorial(accumulator: Int, n: Int): Int =
      if(n == 0) accumulator else factorial(accumulator * n, n -1)

    def calculateValue(): Int =
      if(c < 0 || r < 0) 0
      else factorial(1,r) / (factorial(1,c) * factorial(1,r - c))

    calculateValue()
  }

  /**
   * Exercise 2
   */
  def balance(string: List[Char]): Boolean = {

    def evalChar(char: Char): Int = {
      if(char == '(') 1
      else if (char == ')') - 1
      else 0
    }

    def isBalanced(string: List[Char], open: Int): Boolean = {
      if (open < 0) false
      else {
        val result = evalChar(string.head)
        if (string.tail.isEmpty){
          if (open + result == 0) true
          else false
        }
        else isBalanced(string.tail, open + result)
      }
    }

    isBalanced(string, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      countChange(money, if(coins.tail.isEmpty) List.empty else coins.tail) +
        countChange(money - coins.head, coins)
    }
  }
}
