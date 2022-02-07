package blockchain

import blockchain.HashTree.{Leaf, Node, Twig}
import cats.data.NonEmptyList as Nel
import cats.syntax.all.*

import scala.annotation.tailrec

enum HashTree[+A](val treeHash: Hash):
  case Leaf(value: A, override val treeHash: Hash) extends HashTree(treeHash)
  case Node(left: HashTree[A], override val treeHash: Hash, right: HashTree[A]) extends HashTree(treeHash)
  case Twig(left: HashTree[A], override val treeHash: Hash) extends HashTree(treeHash)

object HashTree:
  def leaf[A: Hashable](a: A): HashTree[A] =
    Leaf(a, a.hash)

  def node[A: Hashable](l: HashTree[A], r: HashTree[A]): HashTree[A] =
    Node(l, l.treeHash |+| r.treeHash, r)

  def twig[A: Hashable](l: HashTree[A]): HashTree[A] =
    Twig(l, l.treeHash |+| l.treeHash)

  def buildTree[A: Hashable](values: List[A]): Option[HashTree[A]] =
    @tailrec
    def go(hashTree: List[HashTree[A]]): Option[HashTree[A]] =
      hashTree match
        case Nil => none
        case x :: Nil => x.some
        case xs => go(go1(xs))

    def go1(hashTree: List[HashTree[A]]): List[HashTree[A]] =
      hashTree match
        case Nil => Nil
        case x :: Nil => List(twig(x))
        case x :: y :: ys => node(x, y) :: go1(ys)

    go(values.map(leaf))

  given[A]: Hashable[HashTree[A]] with
    extension(a: HashTree[A]) def hash: Hash = a.treeHash
