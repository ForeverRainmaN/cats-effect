package com.rockthejvm.part3concurrency

import cats.effect.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}

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

  /**
   * Resources
   */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acquire a connection bases on the file
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open.myDebug >> IO.never
        }(conn => conn.close.void)
      }(scanner => IO("Closing file").myDebug >> IO(scanner.close()))

  // nesting resources are tedious

  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close.void)
  // ... at a later part of your code
  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string $string").myDebug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").myDebug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource)

  def resourceReadFile(path: String): IO[Unit] =
    Resource.make(openFileScanner(path))(scanner => IO(s"Closing file at path: $path").myDebug >> IO(scanner.close()))
      .use { scanner =>
        def readLineByLine(): IO[Unit] =
          if (scanner.hasNextLine) IO(scanner.nextLine()).myDebug
            >> IO.sleep(100.millis)
            >> readLineByLine()
          else IO.unit

        readLineByLine()
      }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  // nested resources
  def connectionFromConfResource(path: String) =
    Resource.make(IO("opening file").myDebug >> openFileScanner(path))(scanner => IO("Closing file").myDebug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void))

  // equivalent
  def connectionFromConfigurationResourceClean(path: String) =
    for {
      scanner <- Resource.make(IO("opening file").myDebug >> openFileScanner(path))(scanner => IO("Closing file").myDebug >> IO(scanner.close()))
      conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void)
    } yield conn

  val openConnection = connectionFromConfResource("src/main/resources/connection").use(conn => conn.open >> IO.never)

  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(1.second) >> IO("canceling").myDebug >> fib.cancel
  } yield ()

  // finalizers to regular IO's
  val ioWithFinalizer = IO("some resource").myDebug.guarantee(IO("freeing resource").myDebug.void)
  val ioWithFinalizer_v2 = IO("some resource").myDebug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").myDebug).void
    case Errored(e) => IO("Nothing to release").myDebug.void
    case Canceled() => IO("resource got canceled, releasing what's left").myDebug.void
  }

  // connection + file will close automatically
  override def run = canceledConnection
}