import sun.nio.cs.CharsetMapping

object BalancedParentheses{

  def balance(string: List[Char]): Boolean = {

    def evalChar(char: Char): Int = {
      if(char == '(') 1
      else if (char == ')') - 1
      else 0
    }

    def isBalanced(string: List[Char], open: Int): Boolean = {
      val result = evalChar(string.head)
      if (string.tail.isEmpty){
        if(open < 0 ) false
        else if (open + result == 0) true
        else false
      }
      else isBalanced(string.tail, open + result)
    }

    isBalanced(string, 0)
  }




  balance("(true)".toList)
  balance("((false".toList)
  balance("(if (zero? x) max (/ 1 x))".toList)
  balance("())(".toList)

}