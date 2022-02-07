package blockchain

import cats.syntax.all.*
import cats.{Semigroup, Show}

trait Hashable[A]:
  extension(a: A)
    def hash: Hash
    def hashWithSalt(h: Hash): Hash = h |+| a.hash

object Hashable:
  given Show[Hash] with
   def show(h: Hash): String = f"$h%#010x"

  // Combine two hash values. 0 is the left identity
  given Semigroup[Hash] with
    def combine(a: Hash, b: Hash): Hash = a * 16777619 + b

  val defaultSalt: Hash = 0xdeadbeef

  given Hashable[Int] with
    extension(a: Int) def hash: Hash = a

  given Hashable[Char] with
    extension(a: Char) def hash: Hash = a.toInt

  given[A: Hashable, B: Hashable]: Hashable[(A, B)] with
    extension(ab: (A, B)) def hash: Hash = ab match
      case (a, b) => a.hash |+| b.hash

  given[A: Hashable]: Hashable[Seq[A]] with
    extension(a: Seq[A]) def hash: Hash = a match
      case Nil => 0
      case x :: Nil => x.hash
      case x::xs => x.hash |+| xs.hash
