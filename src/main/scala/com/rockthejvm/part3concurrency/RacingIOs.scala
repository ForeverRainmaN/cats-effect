package com.rockthejvm.part3concurrency

import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").myDebug >>
        IO.sleep(duration) >>
        IO(s"computation for $value done") >>
        IO(value)
      ).onCancel(IO(s"computation CANCELED for $value").myDebug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
        - both IOs run on separate fibers
        - the first one to finish will complete the result
        - the loser will be canceled
     */
    first.flatMap {
      case Left(mol) => IO(s"meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").myDebug >> IO(outMol).myDebug
      case Right((fiberMol, outLang)) => fiberMol.cancel >> IO("Language won").myDebug >> IO(outLang).myDebug
    }
  }

  override def run = testRace().myDebug.void
}
