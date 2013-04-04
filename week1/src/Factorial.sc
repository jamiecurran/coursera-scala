import annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: jamie
 * Date: 27/03/13
 * Time: 20:33
 * To change this template use File | Settings | File Templates.
 */

object Factorial{
//  def factorial(n: Int): Int =
//    if (n == 0) 1 else n * factorial(n - 1)
//  n = 5
//  5 * 4 * 3 * 2 * 1


  def factorial(n: Int): Int = {

    def loop(acc: Int, n:Int): Int =
      if(n == 0 ) acc
      else loop(acc * n, n - 1)
    loop(1,n)
  }

  // n = 5
  // loop(1,5) = if (5 == 0) 1 else loop(1 * 5, 5 -1)
  // loop(1,5) = loop(5, 4)
  // loop(5,4) = if (4 == 0) 5 else loop(5 * 4, 4 -1)
  // loop(5,4) = loop(20,3)
  // loop(20,3) = if (3 == 0) 20 else loop(20 *3, 3 - 1)
  // loop(20,3) = loop(60,2)
  // loop(60,2) = if (2 == 0) 60 else loop(60 * 2, 2 - 1)
  // loop(60,2) = loop(120,1)
  // loop(120,1) = if (1 == 0) 120 else loop(120 * 1, 1 - 1)
  // loop(120,1) = loop(120,0)
  // loop(120,0) = if (0 == 0) 120

  factorial(5)
}
