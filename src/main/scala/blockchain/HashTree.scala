package blockchain

import blockchain.HashTree.{Leaf, Node, Twig}
import cats.Show
import cats.data.NonEmptyList as Nel
import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

enum HashTree[+A]:
  case Leaf(value: A, hash: Hash)
  case Twig(left: HashTree[A], hash: Hash)
  case Node(left: HashTree[A], hash: Hash, right: HashTree[A])

  def treeHash: Hash = this match
    case Leaf(_, hash) => hash
    case Twig(_, hash) => hash
    case Node(_, hash, _) => hash

object HashTree:
  def leaf[A: Hashable](a: A): HashTree[A] =
    Leaf(a, a.hash)

  def twig[A: Hashable](t: HashTree[A]): HashTree[A] =
    Twig(t, t.treeHash |+| t.treeHash)

  def node[A: Hashable](l: HashTree[A], r: HashTree[A]): HashTree[A] =
    Node(l, l.treeHash |+| r.treeHash, r)

  def buildTree[A: Hashable](values: List[A]): Option[HashTree[A]] =
    def buildList(tree: List[HashTree[A]]): List[HashTree[A]] = tree match
      case Nil => Nil
      case x :: Nil => List(twig(x))
      case x :: y :: ys => node(x, y) :: buildList(ys)

    @tailrec
    def auxBuild(tree: List[HashTree[A]]): Option[HashTree[A]] = tree match
      case Nil => none
      case x :: Nil => x.some
      case xs => auxBuild(buildList(xs))

    auxBuild(values.map(leaf))

  def drawTree[A: Show](tree: HashTree[A]): String =
    def drawNode(tree: HashTree[A], lvl: Int): String = " ".repeat(2 * lvl) ++ (tree match
      case Leaf(value, hash) => s"${hash.show} '${value.show}'$EOL"
      case Twig(_, hash) => s"${hash.show} +$EOL"
      case Node(_, hash, _) => s"${hash.show} -$EOL"
    )

    def auxDraw(tree: HashTree[A], lvl: Int): String = tree match
      case Leaf(_, _) => drawNode(tree, lvl)
      case Twig(t, _) => drawNode(tree, lvl) ++ auxDraw(t, lvl + 1)
      case Node(l, _, r) => drawNode(tree, lvl) ++ auxDraw(l, lvl + 1) ++ auxDraw(r, lvl + 1)

    auxDraw(tree, 0)

  given[A]: Hashable[HashTree[A]] with
    extension(a: HashTree[A]) def hash: Hash = a.treeHash
