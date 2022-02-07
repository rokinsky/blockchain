package blockchain

import cats.{Semigroup, Show}

type Hash = Int

object Hash:
  given Show[Hash] with
    def show(h: Hash): String = f"$h%#010x"

  // Combine two hash values. 0 is the left identity
  given Semigroup[Hash] with
    def combine(a: Hash, b: Hash): Hash = a * 16777619 + b

  val defaultSalt: Hash = 0xdeadbeef
