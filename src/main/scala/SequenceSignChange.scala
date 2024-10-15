object SequenceSignChange extends App {

  def fn(seq: String) = {

    val sequence: Seq[Int] = seq.split("\\s+").flatMap(_.toIntOption.toList)

    val nonZero: Seq[Int] = sequence.filter(_ != 0)

    nonZero.toList match {
      case head :: tail =>
        val res = tail.foldLeft(0 -> head) { case ((total, prev), current) =>
          val signChanged = prev * current < 0
          (if (signChanged) total + 1 else total) -> current
        }

        res._1

      case Nil => 0
    }
  }

  println(fn("11 23 -5 0"))
  println(fn("43 0 23 -2 -3 4 -1"))

}
