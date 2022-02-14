package com.github.rokinsky.client

import cats.effect.{IO, IOApp}
import fs2.io.{stdinUtf8, stdoutLines}

object ClientApp extends IOApp.Simple:
  val run: IO[Unit] =
    Client.start[IO](stdinUtf8(4096), stdoutLines()).compile.drain
