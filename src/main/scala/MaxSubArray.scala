// solution to https://leetcode.com/problems/maximum-subarray
// MEDIUM

object MaxSubArray extends App {
  def maxSubArray(nums: Array[Int]): Int = {

    val currentMax = Integer.MIN_VALUE
    val subSum     = 0

    val res = nums.foldLeft(currentMax -> subSum) { case ((cm, sum), el) =>
      val subSum = if (sum > 0) {
        sum + el
      } else {
        el
      }

      val m = if (cm > subSum) cm else subSum
      m -> subSum
    }

    res._1

  }

  println(maxSubArray(Array(-2, 1)))
}
