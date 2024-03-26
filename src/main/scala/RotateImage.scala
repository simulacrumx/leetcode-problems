// solution to https://leetcode.com/problems/rotate-image
// MEDIUM

object RotateImage extends App {

  def rotate(matrix: Array[Array[Int]]): Array[Array[Int]] = {

    val rank = matrix.length

    for (s <- 0 until rank / 2) {
      val start = s
      val end   = rank - 1 - s

      for (i <- start until end) {

        val shift = i - start

        val upper  = matrix(start)(i)           // to right
        val right  = matrix(i)(end)             // to bottom
        val bottom = matrix(end)(end - shift)   // to left
        val left   = matrix(end - shift)(start) // to up

        matrix(i)(end) = upper
        matrix(end)(end - shift) = right
        matrix(end - shift)(start) = bottom
        matrix(start)(i) = left

      }
    }

    matrix
  }

  println(
    rotate(
      Array(
        Array(5, 1, 9, 11),
        Array(2, 4, 8, 10),
        Array(13, 3, 6, 7),
        Array(15, 14, 12, 16)
      )
    ).toList.map(_.toList)
  )
}
