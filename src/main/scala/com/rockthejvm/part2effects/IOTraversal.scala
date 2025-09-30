package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String) = Future[Int] {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  def workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")

  def clunckyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    futures.foreach(_.foreach(println))
    // Future[List[Int]] would be hard to obtain
  }
  // traverse

  import cats.Traverse
  import cats.instances.list.*

  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation) // all the results in one future!

    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils.*

  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.myDebug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal

  import cats.syntax.parallel.*

  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)

  def sequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  def parSequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)

  // existing sequence API
  val signeIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  val parallelSigneIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSigneIO_v3: IO[List[Int]] = ios.parSequence // extension method from the parallel syntax package

  override def run: IO[Unit] =
    parallelSingleIO.map(_.sum).myDebug.void
}
