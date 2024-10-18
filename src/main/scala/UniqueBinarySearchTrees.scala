// solution to https://leetcode.com/problems/unique-binary-search-trees
// MEDIUM

object UniqueBinarySearchTrees extends App {
  def numTrees(n: Int): Int = {

    // left is less, right is more
    // sorted massive but middle is chosen randomly

    val presets = Map(
      0 -> 0,
      1 -> 1,
      2 -> 2
    )

    def aux(n: Int, prev: Map[Int, Int]): Int = {
      val res = for (i <- 0 until n) yield {
        // amount of elements to the left side
        val left = i
        // amount of elements to the right side
        val right       = n - i - 1
        lazy val leftR  = prev.getOrElse(left, aux(left, prev))
        lazy val rightR = prev.getOrElse(right, aux(right, prev))

        if (left == 0) { rightR }
        else if (right == 0) {
          leftR
        } else {
          leftR * rightR
        }
      }

      res.sum
    }

    presets.getOrElse(n, aux(n, presets))
  }

  println(numTrees(5))
}
