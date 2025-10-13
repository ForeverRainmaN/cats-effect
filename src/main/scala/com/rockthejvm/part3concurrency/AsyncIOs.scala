package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread...")
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  // lift computation to an IO
  // async is a FFI
  val asyncMolIO: IO[Int] = IO.async_ { cb => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaningOfLifeEither()
      cb(result) // CE thread is notified with the result
    }
  }

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
    IO.async_[A] { cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }
  }

  val asyncMol_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  lazy val molFuture: Future[Int] = Future {
    computeMeaningOfLife()
  }(using ec)

  def futureToIO[A](future: => Future[A], ec: ExecutionContext): IO[A] = {
    IO.async_ { cb =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }(using ec)
    }
  }

  val asyncMolIO_v3: IO[Int] = futureToIO(molFuture, ec)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  val neverEndingIO = IO.async_(_ => ()) // no callback, no finish
  val neverEndingIO_v2: IO[Int] = IO.never

  /*
    FULL ASYNC CALL
   */
  import scala.concurrent.duration.*

  def demoAsyncCancelation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { cb =>
      // return IO[Option[IO[Unit]]]
      /*
        finalizer in case computation gets cancelled
        finalizers are of type IO[Unit]
        not specifying finalizer => Option[IO[Unit]]
        creating option is an effect => IO[Option[IO[Unit]]]
       */
      IO {
        threadPool.execute { () => // computation not managed by CE
          val result = computeMeaningOfLifeEither()
          cb(result) // CE thread is notified with the result
        }
      }.as(Some(IO("Cancelled").myDebug.void))
    }


    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("cancelling...").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }


  override def run = demoAsyncCancelation().myDebug >> IO(threadPool.shutdown())
}
