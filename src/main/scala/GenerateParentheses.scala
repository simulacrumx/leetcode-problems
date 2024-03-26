// solution to https://leetcode.com/problems/generate-parentheses
// MEDIUM

object GenerateParentheses extends App {

  def generateParenthesis(n: Int): List[String] = {

    val res = List.range(1, n + 1).foldLeft(Map.empty[Int, Set[String]]) {
      case (evaluated, 1) =>
        val initial = Set("()")
        Map(1 -> initial)

      case (evaluated, l) =>
        val step = aux(l, evaluated)
        evaluated.updated(l, step)
    }

    res.getOrElse(n, Set.empty[String]).toList
  }

  def aux(n: Int, m: Map[Int, Set[String]]): Set[String] = {
    m.toList.flatMap { case (s1, leftSet) =>
      val s2 = n - s1
      if (s2 == 1) {
        val leftSet = m.getOrElse(s1, Set.empty[String])
        leftSet.flatMap { l =>
          Set("(" + l + ")", "()" + l, l + "()")
        }

      } else {
        val rightSet = m.getOrElse(s2, Set.empty[String])
        leftSet.flatMap { l =>
          rightSet.flatMap { r =>
            Set(r + l, l + r)
          }
        }.toSet
      }
    }.toSet
  }

  println(generateParenthesis(3))
}
