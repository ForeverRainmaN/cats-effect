package com.rockthejvm.coordination

import cats.effect.{IO, IOApp, Ref}

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

  val gsMol = atomicMol

  override def run: IO[Unit] = ???
}
