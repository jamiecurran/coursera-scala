object hw1_3{

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      countChange(money, if(coins.tail.isEmpty) List.empty else coins.tail) +
        countChange(money - coins.head, coins)
    }
  }
  countChange(4, List(1,2))
}