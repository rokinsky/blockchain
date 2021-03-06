package com.github.rokinsky.client

import cats.Monad
import cats.Show.catsShowForString
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import com.github.rokinsky.blockchain.{HashTree, MerkleProof}
import fs2.io.{stdinUtf8, stdoutLines}
import fs2.{text, Pipe, Stream}

import scala.util.Properties.lineSeparator as EOL

object Client:
  def program[F[_]: Monad]: F[Option[String]] =
    for
      _ <- HashTree
        .of("fubar".toList)
        .traverse_(_.show.pure)
      paths <- HashTree
        .of("bitcoin".toList)
        .map(tree => MerkleProof.merklePaths('i', tree))
        .map(_.map(MerkleProof.showMerklePath).show)
        .pure[F]
    yield paths

  def start[F[_]: Sync: Console](source: Stream[F, String], emit: Pipe[F, String, Nothing]): Stream[F, Unit] =
    Stream.exec(Console[F].print(s"Input a miner address: ")) ++
      source
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalMapFilter(_ => program)
        .map(_ ++ EOL)
        .through(emit)
