package com.rockthejvm.part3concurrency

import cats.effect.{Fiber, IO, IOApp}

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
  def runOnSomeOtherThread[A](io: IO[A]) = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result


  override def run: IO[Unit] =
    runOnSomeOtherThread(meaninngOfLife) // IO(Succeeded(IO(42)))
      .myDebug.void
}
