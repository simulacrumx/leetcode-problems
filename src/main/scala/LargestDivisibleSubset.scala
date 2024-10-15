object LargestDivisibleSubset extends App {

  def largestDivisibleSubset(nums: Array[Int]): List[Int] = {

    val sorted = nums.sorted

    val ctx = (Map.empty[Int, List[Int]])

    val res = sorted.toList.foldLeft(ctx) { case (acc, el) =>
      val candidates = acc.keys.filter(el % _ == 0)

      if (candidates.isEmpty) {
        acc.updated(el, List(el))
      } else {
        val maxKey = candidates.maxBy(key => acc.getOrElse(key, List.empty).length)
        acc.updated(el, el :: acc.getOrElse(maxKey, List.empty))
      }
    }

    res.values.maxBy(_.length).reverse
  }

  println(largestDivisibleSubset(Array(3, 4, 16, 8)))
}
