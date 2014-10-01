package recfun
import common._
import scala.collection.mutable.ListBuffer


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
  def isEdge(c: Int, r: Int) =
    c == r || c == 0
  def pascal(c: Int, r: Int): Int =
    if (isEdge(c, r)) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {
    def parentheses(elem: Char) =
      if (elem == '(') 1
      else if (elem == ')') -1
      else 0

    def checkList(level: Int, charList: List[Char]): Int =
      if (charList.isEmpty || level == -1) level
      else checkList(level + parentheses(charList.head), charList.tail)

    checkList(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def func(cn: List[(Int, Int)], count: Int): Int = {
      if (cn.isEmpty) {
        count
      } else {
        val b = ListBuffer[(Int, Int)]()
        var nc = count
        for ((lastMaxCoin, total) <- cn) {
          if (total < money) {
            for (c <- coins) {
              if (c >= lastMaxCoin) {
                val e = (c, total + c)
                b += e
              }
            }
          } else if (total == money) nc += 1
          
        }
 
        func(b.toList, nc)
      }
    }
    val b = coins.map { c => (c, c) }
    func(b, 0)
  }

}
