package com.rockthejvm.part3concurrency

import cats.effect.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.utils.*

import scala.concurrent.CancellationException
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

  def timeOut[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val first: IO[Either[A, Unit]] = IO.race(io, IO.sleep(duration))
    first.flatMap {
      case Left(value) => IO(value)
      case Right(_) => IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Succeeded(fb) => fb.map(Right(_))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))
      }

      case Right((fibA, _)) => fibA.join.flatMap {
        case Succeeded(fa) => fa.map(Left(_))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))
      }
    }
  }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB)) =>
        outcomeA match {
          case Succeeded(fa) => fiberB.cancel >> fa.map(Left(_))
          case Errored(e) => fiberB.cancel >> IO.raiseError(e)
          case Canceled() => fiberB.join.flatMap {
            case Succeeded(effectB) => effectB.map(Right(_))
            case Errored(e) => IO.raiseError(e)
            case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled"))
          }
        }

      case Right((fiberA, outcomeB)) =>
        outcomeB match {
          case Succeeded(fb) => fiberA.cancel >> fb.map(Right(_))
          case Errored(e) => fiberA.cancel >> IO.raiseError(e)
          case Canceled() => fiberA.join.flatMap {
            case Succeeded(effectA) => effectA.map(Left(_))
            case Errored(e) => IO.raiseError(e)
            case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled"))
          }
        }
    }
  }

  override def run = ???
}

