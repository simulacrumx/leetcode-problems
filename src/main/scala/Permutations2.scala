// solution to https://leetcode.com/problems/permutations-ii
// MEDIUM

import scala.annotation.tailrec

object Permutations2 extends App {

  def permuteUnique(nums: Array[Int]): List[List[Int]] = {
    val indexSet = List.range(0, nums.length).toSet

    def aux(currentTail: List[Int], availableIndices: Set[Int]): List[List[Int]] = {
      if (availableIndices.isEmpty) {
        List(currentTail)
      } else {
        availableIndices
          .map(i => nums.apply(i) -> i)
          .toList
          .distinctBy(_._1)
          .flatMap { case (v, i) =>
            aux(v :: currentTail, availableIndices - i)
          }
      }
    }

    indexSet.toList.flatMap(i => aux(Nil, indexSet)).distinct
  }

  println(permuteUnique(Array(1, 1, 2)))
}
