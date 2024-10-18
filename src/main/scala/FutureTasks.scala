import scala.concurrent._
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.atomic.AtomicInteger

object FutureTasks extends App {

  def traverse[B, A](
      seq: Seq[B]
  )(f: B => Future[A])(implicit ec: ExecutionContext): Future[Seq[A]] = {
    seq
      .foldLeft(Future.successful(List.empty[A])) { case (fut, el) =>
        for {
          acc <- fut
          a   <- f(el)
        } yield a :: acc
      }
      .map(_.reverse)
  }

  def parTraverse[B, A](
      seq: Seq[B]
  )(f: B => Future[A])(implicit ec: ExecutionContext): Future[Seq[A]] = {
    seq
      .map(f)
      .foldLeft(Future.successful(List.empty[A])) { case (fut, el) =>
        for {
          acc <- fut
          a   <- el
        } yield a :: acc
      }
      .map(_.reverse)
  }

  def racePair[T](f1: => Future[T], f2: => Future[T])(implicit ec: ExecutionContext): Future[T] = {

    val p1   = Promise[T]()
    val lock = new Object {}

    f1.onComplete {
      case Success(value) =>
        lock.synchronized {
          if (p1.isCompleted) {} else p1.success(value)
        }

      case Failure(exception) =>
        lock.synchronized {
          if (p1.isCompleted) {} else p1.failure(exception)
        }
    }

    f2.onComplete {
      case Success(value) =>
        lock.synchronized {
          if (p1.isCompleted) {} else p1.success(value)
        }
      case Failure(exception) =>
        lock.synchronized {
          if (p1.isCompleted) {} else p1.failure(exception)
        }
    }

    p1.future

  }

}
