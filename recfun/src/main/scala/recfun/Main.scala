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
    def factorial(n: Int): BigInt = {
      def loop(n: Int, acc: => BigInt): BigInt = {
        if (n > 1) loop(n - 1, n * acc)
        else acc
      }
      loop(n, 1)
    }
    
    if (c > r || r * c < 0) 0
    else (factorial(r) / (factorial(r - c) * factorial(c))).toInt
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isOpen(x: Char): Boolean = (x == "(".toList.head)

    def isClose(y: Char): Boolean = (y == ")".toList.head)

    def isNeither(z: Char): Boolean =
      !(isOpen(z) || isClose(z))

    def find(chars: List[Char], looking: Int): Boolean = {
      if (chars.isEmpty) (looking == 0)
      else if (isOpen(chars.head)) find(chars.tail, looking + 1)
      else if (isNeither(chars.head)) find(chars.tail, looking)
      else if (isClose(chars.head) && looking > 0) find(chars.tail, looking - 1)
      else !((isClose(chars.head) && looking == 0) || (looking > 0 && chars.tail.isEmpty))
    }
    find(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def makeChange(coins: List[Int], total: Int): Int = {
      def sameNumber(coins: List[Int], total: Int): Int = {
        def loop(coin: Int): Int = {
          if (money % coins.head == 0) 1
          else 0
        }
        if (coins.tail.isEmpty)
          total + loop(coins.head)
        else
          sameNumber(coins.tail, total + loop(coins.head))
      }
      if (!coins.tail.isEmpty) total + sameNumber(coins, 0) + makeChange(coins.tail, 0)
      else total + sameNumber(coins, 0)
    }
    makeChange(coins, 0)
  }
}
