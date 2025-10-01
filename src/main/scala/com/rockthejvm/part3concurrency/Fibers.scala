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

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val res = for {
      fib <- io.start
      result <- fib.join
    } yield result

    res.flatMap {
      case Succeeded(fa) => fa
      case Errored(err) => IO.raiseError(err)
      case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
    }
  }

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val effect = for {
      fib1 <- ioa.start
      fib2 <- iob.start
      result1 <- fib1.join
      result2 <- fib2.join
    } yield (result1, result2)

    effect.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation canceled"))
    }
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) *> IO(1).myDebug
    val secondIO = IO.sleep(3.seconds) *> IO(2).myDebug

    tupleIOs(firstIO, secondIO)
  }

  def timeOut[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful - fibers can leak
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  def testEx3() = {
    val aComputation = IO("Starting").myDebug >> IO.sleep(1.second) >> IO("Done").myDebug >> IO(42)
    timeOut(aComputation, 2.seconds).myDebug.void
  }

  override def run: IO[Unit] = testEx3()
}

/*
val effect = for {
      fib1 <- ioa.start.handleErrorWith(IO.raiseError).onCancel {
        IO.raiseError(new RuntimeException("Cancelled"))
      }
      fib2 <- iob.start.handleErrorWith(IO.raiseError).onCancel {
        IO.raiseError(new RuntimeException("Cancelled"))
      }
      result1 <- fib1.join
      result2 <- fib2.join
    } yield (result1, result2)
 */