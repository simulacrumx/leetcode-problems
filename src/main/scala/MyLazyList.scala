import MyLazyList.MLL.infiniteIntSequence
import MyLazyList.MLL.Cons

object MyLazyList extends App {

  sealed trait MLL[+T] {

    def ::[G >: T](x: G): MLL[G] = {
      MLL.Cons.apply[G](x, () => this)
    }

    def take(amount: Int): List[T] = {
      this match {
        case MLL.Cons(head, tail) if amount > 0 =>
          head :: tail().take(amount - 1)

        case MLL.Cons(head, tail) if amount == 0 =>
          Nil

        case _ => Nil
      }
    }
  }

  object MLL {
    case object EOL                                 extends MLL[Nothing]
    case class Cons[T](head: T, tail: () => MLL[T]) extends MLL[T]

    def infiniteIntSequence: MLL[Int] = {
      def aux(seed: Int): MLL[Int] = Cons(seed, () => aux(seed + 1))
      aux(0)
    }
  }

  println((2 :: MLL.infiniteIntSequence).take(10))
}
