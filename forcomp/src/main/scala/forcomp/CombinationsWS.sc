import forcomp.Anagrams._
object CombinationsWS {

  //def combinations(occurrences: Occurrences): List
  //
  def combinations(occurrences: Occurrences): List[Occurrences] = {
//    if(occurrences.isEmpty) List()
//    else{
      for {
        //rest <- combinations(occurrences.tail)
        (x, y) <- occurrences
        count <- 1 until y
      } yield (x, count)
    }
  //}
  val occurrences: Occurrences = List(('a',2), ('b', 2))




  combinations(occurrences)
  //  val o1 = List(('a', 1), ('a', 2))
  //  val o2 = List(('b', 2), ('b', 1))
  //  o1 zip o2
}

def combo(occurrences: Occurrences): List[Occurrences] = {
  if(occurrences.isEmpty) {
    print("empty\n")
    List(List())
  }
  else {
    for{
      split <- 1 to occurrences.length
      (x,y) <- occurrences take split
      count <- 1 to y
      rest <- combo(occurrences drop split)
    } yield (x,count) :: rest
  }.toList
}

/*
List(
  List((a,1), (b,1)),
  List((a,1), (b,2)),
  List((a,2), (b,1)),
  List((a,2), (b,2)),
  List((a,1)),
  List((a,2)),
  List((b,1)),
  List((b,2)))
*/
/*
List(
  List((a,1), (b,1)),
  List((a,1), (b,2)),
  List((a,2), (b,1)),
  List((a,2), (b,2)),
  List((a,1)),
  List((a,2)),
  List((b,1)),
  List((b,2)))
*/

/*
*    List(
  *      List(),
  *      List(('a', 1)),
  *      List(('a', 2)),
  *      List(('b', 1)),
  *      List(('a', 1), ('b', 1)),
  *      List(('a', 2), ('b', 1)),
  *      List(('b', 2)),
  *      List(('a', 1), ('b', 2)),
  *      List(('a', 2), ('b', 2))
  *      *    )
*/