package com.github.rokinsky.boot

import cats.Monad
import cats.Show.catsShowForChar
import cats.effect.std.Console
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import com.github.rokinsky.blockchain.{HashTree, MerkleProof}
import fs2.io.{stdinUtf8, stdoutLines}
import fs2.{Pipe, Stream, text}

object App:
  def app[F[_]: Monad]: F[Option[String]] =
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

  def stream[F[_]: Sync](source: Stream[F, String], emit: Pipe[F, String, Nothing]): Stream[F, Nothing] =
    stdinUtf8(4096)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMapFilter(_ => app)
      .map(_ ++ "\n")
      .through(stdoutLines())
