// solution to https://leetcode.com/problems/search-a-2d-matrix
// MEDIUM

import scala.annotation.tailrec

object Search2DMatrix extends App {

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {

    def idxSearchBuilder(m: Int, matrix: Array[Array[Int]])(i: Int): Int =
      matrix(i / m)(i % m)

    val n = matrix.length
    val m = matrix.lift(0).map(_.length).getOrElse(0)

    val size = m * n

    val idx = idxSearchBuilder(m, matrix) _

    @tailrec
    def aux(start: Int, end: Int, target: Int): Boolean = {
      if (end - start == 0) {
        target == idx(start)
      } else {
        val pivot  = (end + start) / 2
        val pivotV = idx(pivot)

        // println(start, end, pivot)
        if (pivotV < target) {
          aux(pivot + 1, end, target)
        } else if (pivotV > target) {
          aux(start, pivot, target)
        } else true
      }
    }

    aux(0, size - 1, target)
  }

  println(
    searchMatrix(
      Array(
        Array(1, 3, 5, 7),
        Array(10, 11, 16, 20),
        Array(23, 30, 34, 60)
      ),
      3
    )
  )
}
