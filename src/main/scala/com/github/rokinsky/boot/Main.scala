package com.github.rokinsky.boot

import cats.effect.{IO, IOApp}
import fs2.io.{stdinUtf8, stdoutLines}

object Main extends IOApp.Simple:
  val run: IO[Unit] =
    App.stream[IO](stdinUtf8(4096), stdoutLines()).compile.drain
