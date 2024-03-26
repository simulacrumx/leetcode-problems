// solution to https://leetcode.com/problems/minimum-path-sum
// MEDIUM

object MinimumPathSum extends App {
  def minPathSum(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid(0).length

    val paths = Array.fill[Int](m, n)(Int.MaxValue)

    paths(0)(0) = grid(0)(0)

    for (i <- 1 until n) {
      paths(0)(i) = paths(0)(i - 1) + grid(0)(i)
    }

    for (j <- 1 until m) {
      paths(j)(0) = paths(j - 1)(0) + grid(j)(0)
    }

    for (j <- 1 until m; i <- 1 until n) {
      val upper = paths(j - 1)(i)
      val left  = paths(j)(i - 1)

      paths(j)(i) = Math.min(
        grid(j)(i) + Math.min(upper, left),
        paths(j)(i)
      )

    }

    paths(m - 1)(n - 1)
  }

  println(minPathSum(Array(Array(1, 3, 1), Array(1, 5, 1), Array(4, 2, 1))))
  println(minPathSum(Array(Array(1, 2, 3), Array(4, 5, 6))))
}
