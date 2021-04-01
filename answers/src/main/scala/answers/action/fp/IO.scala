package answers.action.fp

import scala.util.{Failure, Success, Try}

sealed trait IO[A] {

  def unsafeRun(): A

  def andThen[Other](other: IO[Other]): IO[Other] =
    IO {
      this.unsafeRun()
      other.unsafeRun()
    }

  def onError[Other](callback: Throwable => IO[Other]): IO[A] =
    attempt.flatMap {
      case Failure(e)     => callback(e).attempt *> IO.fail(e)
      case Success(value) => IO(value)
    }

  def flatMap[Other](callBack: A => IO[Other]): IO[Other] =
    IO {
      val result: A             = unsafeRun()
      val nextAction: IO[Other] = callBack(result)

      nextAction.unsafeRun()
    }

  def map[Other](callBack: A => Other): IO[Other] =
    flatMap(a => IO(callBack(a)))

  def retry(maxAttempt: Int): IO[A] =
    if (maxAttempt <= 0) IO.fail(new IllegalArgumentException("maxAttempt must be > 0"))
    else if (maxAttempt == 1) this
    else
      attempt.flatMap {
        case Failure(_)     => retry(maxAttempt - 1)
        case Success(value) => IO(value)
      }

  def *>[Next](next: IO[Next]): IO[Next] =
    this.flatMap(_ => next)

  def attempt: IO[Try[A]] =
    IO {
      Try(unsafeRun())
    }
}

object IO {
  def apply[A](block: => A): IO[A] =
    new IO[A] {
      def unsafeRun(): A = block
    }

  def fail[A](error: Throwable): IO[A] =
    IO(throw error)

}