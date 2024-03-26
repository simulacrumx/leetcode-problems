// solution to https://leetcode.com/problems/unique-paths/
// MEDIUM

object UniquePaths extends App {
  def uniquePaths(m: Int, n: Int): Int = {
    val grid = Array.ofDim[Int](m, n)
    grid(0)(0) = 1

    for (j <- 0 until m) {
      for (i <- 0 until n) {
        if (i < n - 1) {
          grid(j)(i + 1) = grid(j)(i + 1) + grid(j)(i)

        }
        if (j < m - 1) {
          grid(j + 1)(i) = grid(j + 1)(i) + grid(j)(i)
        }
      }
    }

    grid(m - 1)(n - 1)
  }

  println(uniquePaths(3, 7))

}
