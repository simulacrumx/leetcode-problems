// solution to https://leetcode.com/problems/minimum-length-of-string-after-deleting-similar-ends
// MEDIUM

import scala.annotation.tailrec

object MinimumLengthOfString extends App {

  def minimumLength(s: String): Int = {
    val chars  = s.toCharArray()
    val length = chars.length

    @tailrec
    def searchFromLeft(start: Int, end: Int, target: Char): Int = {
      if (start + 1 < end) {
        val cursor = chars(start)
        if (cursor == target) searchFromLeft(start + 1, end, target) else start
      } else start
    }

    @tailrec
    def searchFromRight(start: Int, end: Int, target: Char): Int = {
      if (end > start + 1) {
        val cursor = chars(end)
        if (cursor == target) searchFromRight(start, end - 1, target) else end
      } else end
    }

    @tailrec
    def aux(start: Int, end: Int): Int = {
      // println("checking:", chars(start), chars(end))
      if (chars(start) == chars(end) && start < end) {

        end - start match {
          case 1 => 0
          case 2 => 1
          case _ =>
            val left  = searchFromLeft(start, end, chars(start))
            val right = searchFromRight(left, end, chars(end))
            aux(left, right)
        }
      } else {
        end - start + 1
      }
    }

    aux(0, length - 1)
  }

  println(minimumLength("ca"))
  println(minimumLength("cabaabacc"))
  println(minimumLength("aabccabba"))
  println(minimumLength("abbbbbbbbbbbbbbbbbbba"))
  println(minimumLength("bbbbbbbbbbbbbbbbbbbbbbbbbbbabbbbbbbbbbbbbbbccbcbcbccbbabbb"))
  println(minimumLength("cabacbac"))
}
