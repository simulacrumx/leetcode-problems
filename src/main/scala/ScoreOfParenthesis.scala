// solution to https://leetcode.com/problems/score-of-parentheses
// MEDIUM

object ScoreOfParenthesis extends App {
  def scoreOfParentheses(s: String): Int = {

    def findCompleteSubstr(s: String): String = {

      val res = (0 until s.length).foldLeft(Option.empty[String] -> 0) {
        case ((Some(s), _), _) => Some(s) -> 0
        case ((None, sum), i) =>
          val add = if (s(i) == '(') 1 else -1
          val upd = sum + add
          if (upd == 0) {
            (Some(s.substring(0, i + 1)) -> upd)
          } else None -> upd
      }

      res._1.getOrElse("")
    }

    val left = findCompleteSubstr(s)
    val leftScore = left match {
      case ""   => 0
      case "()" => 1
      case s    => 2 * scoreOfParentheses(s.substring(1, s.length - 1))
    }

    val right      = s.substring(left.length)
    val rightScore = if (right.length == 0) 0 else scoreOfParentheses(right)

    leftScore + rightScore
  }

  println(scoreOfParentheses("(())"))
  println(scoreOfParentheses("(()())"))
}
