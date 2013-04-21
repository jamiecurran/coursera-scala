package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def emptySet:Set = x => false
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }


  test("singletonSet singletons"){
    new TestSets {
      assert(!contains(s1, 2), "singletonSet(1) does not contain 2")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersects contain both elements") {
    new TestSets {
      val unionSet1 = union(s1,s1)
      val unionSet2 = union(s1,s2)
      val intersect1 = intersect(unionSet1, unionSet2)
      assert(contains(intersect1, 1))
      assert(!contains(intersect1, 2))
    }
  }

  test("diff contains elements in one set and not the other"){
    new TestSets {
      val diff1 = diff(s1, s2)
      assert(contains(diff1, 1))
      assert(!contains(diff1, 2))
    }
  }

  test("filter returns s for which p holds true"){
    new TestSets {
      val union1 = union(s1, s2)
      val filter1 = filter(union1, (x: Int) => x == 2)
      assert(!contains(filter1, 1), "doesn't contain 1")
      assert(contains(filter1, 2), "does contain 2")
    }
    new TestSets {
      val s4 = singletonSet(10)
      val union1 = union(s1,s2)
      val union2 = union(s3,s4)
      val union3 = union(union1, union2)
      val resSet = filter(union3, (x:Int) => x%2 == 0)
      assert(contains(resSet, 2), "mod 2 filter returns set containing 2")
      assert(contains(resSet, 10), "mod 2 filter returns set containing 10")
      assert(!contains(resSet, 12), "mod 2 filter doesn't return set containing 12 as wasn't in the original set")
      assert(!contains(resSet, 1), "mod 2 filter doesn't return set containing 1")
      assert(!contains(resSet, 3), "mod 2 filter doesn't return set containing 3")
    }
  }

  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val s4 = singletonSet(10)
      val union1 = union(s2, s4)
      assert(forall(union1, (x: Int) => x%2 == 0), "all even numbers is true")

      val union2 = union(s1,s2)
      val union3 = union(union2, s3)
      assert(!forall(union3, (x: Int) => x%2 == 0), "all even numbers is false")

      assert(forall(emptySet, x => x%2 == 0), "true for empty set")
    }
  }

  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      val set = union(singletonSet(2), singletonSet(10))
      assert(exists(set, x => x == 2 ), "set contains 2.")
      assert(!exists(set, x => x == 3 ), "set doesn't contains 3.")
      assert(exists(set, x => x == 10 ), "set contains 10.")
      assert(exists(set, x => x%2 == 0), "at least one number is even")
      assert(!exists(emptySet, x => x == 0), "returns false for empty set")
    }
  }

  test("map returns a set transformed by applying `f` to each element of `s`.") {
    val set = union(singletonSet(3), union(singletonSet(2), singletonSet(10)))
    val mapRes = map(set, x => x * 2)
    assert(contains(mapRes, 6), "mapped result contains double of 3, 6")
    assert(contains(mapRes, 4), "mapped result contains double of 2, 4")
    assert(contains(mapRes, 20), "mapped result contains double of 10, 20")
    assert(!contains(mapRes, 3), "mapped result doesn't contain original value of 3")
    assert(!contains(mapRes, 2), "mapped result doesn't contain original value of 2")
    assert(!contains(mapRes, 10), "mapped result doesn't contain original value of 10")

  }

}
