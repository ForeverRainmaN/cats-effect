package com.rockthejvm.coordination

import cats.effect.{Deferred, IO, IOApp, Ref}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple {

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // ge`t blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get // blocks the fiber
  }

  val writed = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[consumer] waiting for result...").myDebug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").myDebug
    } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[producer] crunching numbers...").myDebug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").myDebug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala ", "with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
          IO(s"[downloader] got '$part'").myDebug >> IO.sleep(1.second) >> contentRef.update(currentPart => currentPart + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] File download complete").myDebug
      else IO("[notifier] downloading...").myDebug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef) // busy wait
    } yield ()


    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  // deferred works miracles for waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").myDebug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] File download complete").myDebug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").myDebug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentPart => currentPart + part)
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  override def run: IO[Unit] = fileNotifierWithDeferred()
}
