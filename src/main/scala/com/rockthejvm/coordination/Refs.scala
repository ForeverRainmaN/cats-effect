package com.rockthejvm.coordination

import cats.effect.{IO, IOApp, Ref}
import com.rockthejvm.utils.*

object Refs extends IOApp.Simple {
  // ref = purely functional atomic reference

  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying is an effect
  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43) // thread-safe
  }

  // obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread-safe
  }

  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  } // gets the old value, sets the new one

  // updating with a function
  val fMol: IO[Unit] = atomicMol.flatMap { ref => ref.update(value => value * 10) }

  val updatedMol: IO[Int] = atomicMol.flatMap { ref => ref.updateAndGet(value => value * 10) } // get the new value
  // can also use getAndUpdate to get the OLD value

  // modifying with a function returning a different type

  // modifying with a function returning a different type
  val modifiedMol: IO[String] = atomicMol.flatMap { ref => ref.modify(value => (value * 10, s"my current value is $value")) }

  // why: concurrent + thread-safe reads / writes over shared values, in a purely functional way

  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").myDebug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
        _ = IO(count += newCount)
      } yield ()
    }

    List("I love Cats Effect", "This ref thing is useless", "Daniel writes a lot of code")
      .map(task)
      .parSequence
      .void
  }

  /*
    Drawbacks:
  - hard to read/debug
  - mix pure/impure code
  - not thread safe
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").myDebug
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This ref thing is useless", "Daniel writes a lot of code")
        .map(string => task(string, initialCount))
        .parSequence
    } yield ()
  }

  import scala.concurrent.duration.*

  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- IO(ticks += 1) // not thread safe
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure: IO[Unit] = {
    def tickingClock(ref: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- ref.update(_ + 1)
      _ <- tickingClock(ref)
    } yield ()

    def printTicks(ref: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      value <- ref.get
      _ <- IO(s"TICKS: $value").myDebug
      _ <- printTicks(ref)
    } yield ()

    for {
      ref <- IO.ref(0)
      _ <- (tickingClock(ref), printTicks(ref)).parTupled
    } yield ()
  }

  def tickingClockWeird(): IO[Unit] = {
    val ticks = Ref[IO].of(0)

    def tickingClock: IO[Unit] = for {
      t <- ticks // will give you a NEW REF
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- t.update(_ + 1) // not thread safe
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks // will give you a NEW REF
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }


  override def run: IO[Unit] = tickingClockPure

}
