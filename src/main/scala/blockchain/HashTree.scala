package blockchain

import blockchain.Hash.given_Show_Hash
import blockchain.HashTree.{Leaf, Node, Twig}
import blockchain.Hashable.given_Hashable_A_B
import cats.Show
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.show.*

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

enum HashTree[+A]:
  case Leaf(value: A, hash: Hash)
  case Twig(child: HashTree[A], hash: Hash)
  case Node(left: HashTree[A], hash: Hash, right: HashTree[A])

  def treeHash: Hash = this match
    case Leaf(_, hash) => hash
    case Twig(_, hash) => hash
    case Node(_, hash, _) => hash

object HashTree:

  def leaf[A: Hashable](value: A): HashTree[A] =
    Leaf(value, value.hash)

  def twig[A: Hashable](child: HashTree[A]): HashTree[A] =
    Twig(child, (child.treeHash, child.treeHash).hash)

  def node[A: Hashable](left: HashTree[A], right: HashTree[A]): HashTree[A] =
    Node(left, (left.treeHash, right.treeHash).hash, right)

  def buildTree[A: Hashable](values: List[A]): Option[HashTree[A]] =
    def buildList(tree: List[HashTree[A]]): List[HashTree[A]] = tree match
      case Nil => Nil
      case x :: Nil => List(twig(x))
      case x :: y :: ys => node(x, y) :: buildList(ys)

    @tailrec
    def build(tree: List[HashTree[A]]): Option[HashTree[A]] = tree match
      case Nil => none
      case x :: Nil => x.some
      case xs => build(buildList(xs))

    build(values.map(leaf))

  def drawTree[A: Show](tree: HashTree[A]): String =
    def drawNode(tree: HashTree[A], lvl: Int): String = " ".repeat(2 * lvl) ++ (tree match
      case Leaf(value, hash) => s"${hash.show} '${value.show}'$EOL"
      case Twig(_, hash) => s"${hash.show} +$EOL"
      case Node(_, hash, _) => s"${hash.show} -$EOL"
    )

    def draw(tree: HashTree[A], lvl: Int): String = tree match
      case Leaf(_, _) => drawNode(tree, lvl)
      case Twig(t, _) => drawNode(tree, lvl) ++ draw(t, lvl + 1)
      case Node(l, _, r) => drawNode(tree, lvl) ++ draw(l, lvl + 1) ++ draw(r, lvl + 1)

    draw(tree, 0)

  given[A: Show]: Show[HashTree[A]] with
    def show(a: HashTree[A]): String = drawTree(a)

  given[A]: Hashable[HashTree[A]] with
    extension(a: HashTree[A]) def hash: Hash = a.treeHash
