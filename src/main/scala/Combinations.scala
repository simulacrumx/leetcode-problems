// solution to https://leetcode.com/problems/combinations/
// MEDIUM

object Combinations extends App {

  def combine(n: Int, k: Int): List[List[Int]] = {
    def aux(rem: Int, candidates: Set[Int], tail: List[Int]): List[List[Int]] = {

      rem match {
        case 0 => List(tail)
        case l =>
          val (_, res) = candidates.toList.foldLeft(candidates -> List.empty[List[Int]]) {
            case ((candidates0, acc), el) =>
              (candidates0 - el) -> (acc ::: aux(l - 1, candidates0 - el, el :: tail))
          }

          res
      }
    }

    aux(k, (1 to n).toSet, Nil)
  }

  println(combine(4, 2))
}
