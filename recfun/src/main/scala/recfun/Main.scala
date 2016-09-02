package recfun

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
	  if (c < 0 || r < 0) 0 else 
      if (c == 0 && r == 0) 1 else 
        pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def maintainOpenBraceCount(chars: List[Char], openBraceCount: Int): Int = {
      if(chars.isEmpty || openBraceCount < 0) 
        openBraceCount 
      else{
        if(chars.head == '(') maintainOpenBraceCount(chars.tail, openBraceCount + 1) 
        else 
          if(chars.head == ')') maintainOpenBraceCount(chars.tail, openBraceCount - 1) 
          else maintainOpenBraceCount(chars.tail, openBraceCount)
      }
    }
    maintainOpenBraceCount(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val count: Int = if(coins.isEmpty) 0
    else if(money == 0) 1
    else if (money < 0) 0
    else{
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
    count
  }
}
