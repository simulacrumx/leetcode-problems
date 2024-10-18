// solution to https://leetcode.com/problems/increasing-triplet-subsequence
// MEDIUM
object IncreasingTripletSubseq extends App {

  def increasingTriplet(nums: Array[Int]): Boolean = {

    val res = nums.foldLeft((Option.empty[Int], Option.empty[Int], false)) {
      case ((min, secondMin, cond), el) =>
        val cond0 = for {
          min0       <- min
          secondMin0 <- secondMin
        } yield el > min0 && el > secondMin0 && min0 != secondMin0

        val newMin = min.map(m => if (el < m) el else m).orElse(Some(el))
        val newSecondMin = (for {
          min0 <- min
          sm0  <- secondMin
        } yield if (el > min0 && el < sm0) el else sm0)
          .orElse(newMin.flatMap(m => if (el == m) None else Some(el)))

        (newMin, newSecondMin, cond0.getOrElse(false) || cond)
    }

    res._3
  }

  println(increasingTriplet(Array(1, 2, 3, 4, 5)))
  println(increasingTriplet(Array(5, 4, 3, 2, 1)))
  println(increasingTriplet(Array(2, 1, 5, 0, 4, 6)))
  println(increasingTriplet(Array(2, 4, -2, 3)))
  println(increasingTriplet(Array(20, 100, 10, 12, 5, 13)))

}
