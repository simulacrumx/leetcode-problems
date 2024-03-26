// solution to https://leetcode.com/problems/edit-distance/
// MEDIUM

// levinstein distance

object EditDistance extends App {

  def minDistance(word1: String, word2: String): Int = {

    val wordArr1 = word1.toCharArray()
    val wordArr2 = word2.toCharArray()

    val grid = Array.ofDim[Int](word1.length + 1, word2.length + 1)

    // filling grid of distance between prefixes, including empty prefix

    // filling distance between empty prefix 1 and prefixes of word 2
    for (i <- 0 to word1.length) { grid(i)(0) = i }

    // filling distance between prefixes of word 1 and empty prefix 2
    for (j <- 0 to word2.length) { grid(0)(j) = j }

    // filling grid
    for (i <- 1 to word1.length; j <- 1 to word2.length) {

      // deriving from left cell means next prefix of second word
      // next prefix means 1 more operation to remove it

      val leftV = grid(i)(j - 1) + 1

      // deriving from upper cell means next prefix of first word
      // next prefix means 1 more operation to remove it

      val upperV = grid(i - 1)(j) + 1

      // deriving from upper cell
      // deriving from upper left cell
      // lookahead of next chars of word1 & word2
      // if chars are the same no operation needed,
      // if chars are different 1 more operation needed

      val diagonalV = grid(i - 1)(j - 1) + (if (wordArr1(i - 1) == wordArr2(j - 1)) 0 else 1)

      grid(i)(j) = Math.min(Math.min(leftV, upperV), diagonalV)
    }

    grid(word1.length)(word2.length)
  }

  println(minDistance("horse", "ros"))
  println(minDistance("execution", "intention"))
}
