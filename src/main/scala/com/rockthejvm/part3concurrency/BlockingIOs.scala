package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).myDebug // SEMANTIC BLOCKING
    _ <- IO.sleep(1.second).myDebug
  } yield ()

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread poll specific for blocking calls

  // yielding
  val iosOnManyThreads = for {
    _ <- IO("first").myDebug
    _ <- IO.cede // a signal to yield control over the thread
    _ <- IO("second").myDebug // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").myDebug
  } yield ()

  def testThousandEffectsSwitch() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.myDebug >> IO.cede >> _.myDebug).evalOn(ec)
  }

  /*
    - blocking calls & IO.sleep yield control over the calling thread automatically
   */

  override def run: IO[Unit] = testThousandEffectsSwitch().void

}
