package com.github.rokinsky.blockchain

import cats.syntax.semigroup.*
import cats.{Semigroup, Show}
import com.github.rokinsky.blockchain.Hash.given_Semigroup_Hash

trait Hashable[A]:
  extension(a: A)
    def hash: Hash
    def hashWithSalt(h: Hash): Hash = h |+| a.hash

object Hashable:
  given Hashable[Int] with
    extension(int: Int) def hash: Hash = int

  given Hashable[Char] with
    extension(char: Char) def hash: Hash = char.toInt

  given[A: Hashable, B: Hashable]: Hashable[(A, B)] with
    extension(ab: (A, B)) def hash: Hash = ab match
      case (a, b) => a.hash |+| b.hash

  // FIXME: with default given name it doesn't work
  given given_Hashable_Seq_A[A: Hashable]: Hashable[Seq[A]] with
    extension(a: Seq[A]) def hash: Hash = a match
      case Nil => 0
      case head :: Nil => head.hash
      case head::tail => head.hash |+| tail.hash

  given Hashable[String] with
    extension(string: String) def hash: Hash = string.toList.hash
