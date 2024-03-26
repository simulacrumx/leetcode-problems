// solution to https://leetcode.com/problems/unique-paths-ii/
// MEDIUM

object UniquePathsWithObstacles extends App {
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {

    val m = obstacleGrid.length
    val n = obstacleGrid(0).length

    val grid = Array.ofDim[Int](m, n)
    grid(0)(0) = if (obstacleGrid(0)(0) == 1) 0 else 1

    for (j <- 0 until m) {

      for (i <- 0 until n) {
        if (i < n - 1) {
          if (obstacleGrid(j)(i + 1) == 0) {
            grid(j)(i + 1) = grid(j)(i + 1) + grid(j)(i)
          }
        }

        if (j < m - 1) {
          if (obstacleGrid(j + 1)(i) == 0) {
            grid(j + 1)(i) = grid(j + 1)(i) + grid(j)(i)
          }
        }
      }
    }

    grid(m - 1)(n - 1)

  }

  println(
    uniquePathsWithObstacles(
      Array(
        Array(0, 0, 0, 0),
        Array(0, 1, 0, 0),
        Array(0, 0, 0, 0),
        Array(0, 0, 1, 0),
        Array(0, 0, 0, 0)
      )
    )
  )
}
