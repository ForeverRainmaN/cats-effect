package com.rockthejvm.part3concurrency

import cats.effect.*
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

import scala.concurrent.duration.*


object Fibers extends IOApp.Simple {

  val meaninngOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import com.rockthejvm.utils.*

  def sameThreadIOs(): IO[Unit] = for {
    _ <- meaninngOfLife.myDebug
    _ <- favLang.myDebug
  } yield ()

  // introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaninngOfLife.myDebug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.myDebug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result

  /*
    IO[ResultType of fib.join]
    fib.join = Outcome[IO, Throwable, A]

    possible outcomes:
    - success with an IO
    - failure with an exception
    - canceled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaninngOfLife)

  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").myDebug >> IO.sleep(1.second) >> IO("done").myDebug
    // onCancel is a "finalizer", allowing you to free up resources in case you get cancelled
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled!").myDebug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("canceling").myDebug // running on the calling thread
      _ <- fib.cancel // sends cancellation signal from the calling thread
      result <- fib.join // waits the result in the calling thread
    } yield result
  }

  override def run: IO[Unit] = testCancel().myDebug.void
}
