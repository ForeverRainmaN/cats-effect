package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object CancellingIOs extends IOApp.Simple {

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */

  val chainOfIOs: IO[Int] = IO("waiting").myDebug >> IO.canceled >> IO(42).myDebug

  // uncancelable
  // example: online store, payment processor
  // payment process must NOT be canceled

  val specialPaymentSystem = (
    IO("Payment running, don't cancel me").myDebug >>
      IO.sleep(1.second) >>
      IO("Payment completed").myDebug
    ).onCancel(IO("MEGA CANCEL OF DOOM!").myDebug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").myDebug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    The uncancelable API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
    The poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
    Example: authentication service. Has two parts:
      - input password, can be cancelled, because otherwise we might block indefinitely on user input
      - verify password, CANNOT be cancelled once it's started
   */

  val inputPassword = IO("Input password:").myDebug >> IO("(typing password)").myDebug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")
  val verifyPassword = (pw: String) => IO("verifying...").myDebug >> IO.sleep(5.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later").myDebug.void) // this is cancelable
      verified <- verifyPassword(pw) // this is not cancelable
      _ <- if (verified) IO("Authentication successful").myDebug else IO("Authentication failed").myDebug // this is not cancelable
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
    Uncancelable calls are MASKS which suppress cancellation.
    Poll Ccalls are "gaps opened" in the uncancelable region.
   */

  override def run = authProgram

}
