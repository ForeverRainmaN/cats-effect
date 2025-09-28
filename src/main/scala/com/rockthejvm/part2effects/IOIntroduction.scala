package com.rockthejvm.part2effects

import cats.effect.IO

import scala.annotation.tailrec
import scala.io.StdIn

object IOIntroduction {
  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay({
    println("I'm producing an integer")
    54
  })

  val shouldNotDoThis: IO[Int] = IO.pure({
    println("I'm producing an integer")
    54
  })

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I'm producing an integer")
    42
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO Effects as tuples

  import cats.syntax.apply.*

  val combinedMeaningOfLIfe = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  def sequenceTakeLast_2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // "andThen"

  def sequenceTakeLast_3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // "andThen" with by-name call

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io)

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // don't use this

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] =
    ioa.void // encouraged

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = {
    @tailrec
    def go(remaining: Int, acc: IO[Int]): IO[Int] = {
      if (remaining <= 0) acc
      else go(remaining - 1, acc.map(x => x + remaining))
    }

    go(n, IO.pure(0))
  }

  def sumIO_v2(n: Int): IO[Int] = {
    if (n <= 0) IO(0)
    else sumIO_v2(n - 1).flatMap(x => IO(x + n))
  }

  def sumIO_v3(n: Int): IO[Int] = {
    if (n <= 0) IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield prevSum + lastNumber
  }

  def fibonacci(n: Int): IO[BigInt] =
    if (n <= 2) IO.pure(1)
    else IO.defer {
      for {
        x <- fibonacci(n - 1)
        y <- fibonacci(n - 2)
      } yield x + y
    }

  def fibonacci_v2(n: Int): IO[BigInt] =
    if (n <= 2) IO(1)
    else for {
      last <- IO.defer(fibonacci_v2(n - 1)) // same as .delay(...).flatten
      prev <- IO.defer(fibonacci_v2(n - 2))
    } yield last + prev


  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    forever(IO(println("govno"))).unsafeRunSync()
  }


}
