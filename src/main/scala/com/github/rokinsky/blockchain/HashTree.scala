package com.github.rokinsky.blockchain

import cats.Show
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.show.*
import com.github.rokinsky.blockchain.Hash.given_Show_Hash
import com.github.rokinsky.blockchain.HashTree.{Leaf, Node, Twig}
import com.github.rokinsky.blockchain.Hashable.given_Hashable_A_B

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

enum HashTree[+A]:
  case Leaf(value: A, hash: Hash)
  case Twig(child: HashTree[A], hash: Hash)
  case Node(left: HashTree[A], hash: Hash, right: HashTree[A])

  def treeHash: Hash = this match
    case Leaf(_, hash)    => hash
    case Twig(_, hash)    => hash
    case Node(_, hash, _) => hash

object HashTree:

  def leaf[A: Hashable](value: A): HashTree[A] =
    Leaf(value, value.hash)

  def twig[A: Hashable](child: HashTree[A]): HashTree[A] =
    Twig(child, (child.treeHash, child.treeHash).hash)

  def node[A: Hashable](left: HashTree[A], right: HashTree[A]): HashTree[A] =
    Node(left, (left.treeHash, right.treeHash).hash, right)

  def of[A: Hashable](values: List[A]): Option[HashTree[A]] =
    @tailrec
    def buildList(trees: List[HashTree[A]], acc: List[HashTree[A]] = Nil): List[HashTree[A]] = trees match
      case Nil                   => acc
      case child :: Nil          => twig(child) :: acc
      case left :: right :: tail => buildList(tail, node(left, right) :: acc)

    @tailrec
    def buildTree(trees: List[HashTree[A]]): Option[HashTree[A]] = trees match
      case Nil         => none // the initial tree list shouldn't be empty
      case root :: Nil => root.some
      case _           => buildTree(buildList(trees).reverse)

    buildTree(values.map(leaf))

  given [A: Show]: Show[HashTree[A]] with
    def show(tree: HashTree[A]): String =
      def drawNode(tree: HashTree[A], lvl: Int): String = " ".repeat(2 * lvl) ++ (tree match
        case Leaf(value, hash) => s"${hash.show} '${value.show}'$EOL"
        case Twig(_, hash)     => s"${hash.show} +$EOL"
        case Node(_, hash, _)  => s"${hash.show} -$EOL"
      )

      def drawTree(tree: HashTree[A], lvl: Int): String = tree match
        case Leaf(_, _)           => drawNode(tree, lvl)
        case Twig(child, _)       => drawNode(tree, lvl) ++ drawTree(child, lvl + 1)
        case Node(left, _, right) => drawNode(tree, lvl) ++ drawTree(left, lvl + 1) ++ drawTree(right, lvl + 1)

      drawTree(tree, 0)

  given [A]: Hashable[HashTree[A]] with
    extension (tree: HashTree[A]) def hash: Hash = tree.treeHash
