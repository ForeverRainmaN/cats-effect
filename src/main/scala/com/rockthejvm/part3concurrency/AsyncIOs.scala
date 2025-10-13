package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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
      future.onComplete {
        case Success(value) => cb(Right(value))
        case Failure(exception) => cb(Left(exception))
      }(using ec)
    }
  }


  override def run = asyncMolIO.myDebug >> IO(threadPool.shutdown())
}
