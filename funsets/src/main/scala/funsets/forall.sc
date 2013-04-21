object forall{

  type Set = Int => Boolean

  val bound = 10

  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def union(s: Set, t: Set): Set = (x:Int) => contains(s, x) || contains(t ,x)

  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)

  def emptySet:Set = x => false

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a > bound) true
      else if(contains(s,a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }
  val set = union(singletonSet(2), singletonSet(10))
//  forall(set, (x: Int) => x%2 == 0)
//  forall(set, (x: Int) => x%3 == 0)
//  forall(emptySet, x => x%2 == 0)
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x=> !p(x))


  exists(set, x => x == 2 )



  exists(set, x => x == 3 )


  exists(set, x => x == 10 )







}