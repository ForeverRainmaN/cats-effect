package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple {

  import com.rockthejvm.utils.*

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").myDebug

    def close: IO[String] = IO(s"closing connection to $url").myDebug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open *> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  // problem: Leaking resources
  val correctAsyncFetchUrl = for {
    connection <- IO(new Connection("rockthejvm.com"))
    fib <- (connection.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(connection.close.void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /*
      bracket pattern: someIO.bracket(useResourceCB)(releaseResourceCB)
      bracket is equivalent to try-catches (pure fp)
   */
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.seconds) *> fib.cancel
  } yield ()

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFileImproved(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      def readLoop: IO[Unit] =
        IO(scanner.hasNextLine).flatMap {
          case true =>
            IO(scanner.nextLine()).flatMap { line =>
              IO.println(line) >>
                IO.sleep(100.millis) >>
                readLoop
            }
          case false => IO.unit
        }

      readLoop
    } { scanner =>
      IO(scanner.close()).handleErrorWith(_ => IO.unit)
    }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      openFileScanner(path).bracket { scanner =>
        def readLineByLine(): IO[Unit] =
          if (scanner.hasNextLine) IO(scanner.nextLine()).myDebug
            >> IO.sleep(100.millis)
            >> readLineByLine()
          else IO.unit

        readLineByLine()
      } { scanner =>
        IO(s"Closing file at $path").myDebug >> IO(scanner.close())
      }

  override def run = bracketReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")

}