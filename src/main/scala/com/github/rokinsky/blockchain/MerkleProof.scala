package com.github.rokinsky.blockchain

import cats.syntax.semigroup.*
import cats.syntax.show.*
import com.github.rokinsky.blockchain.Hash.{given_Semigroup_Hash, given_Show_Hash}
import com.github.rokinsky.blockchain.HashTree.{Leaf, Node, Twig}
import com.github.rokinsky.blockchain.MerkleProof.MerklePath

final case class MerkleProof[A](value: A, path: MerklePath)

object MerkleProof:
  type MerklePath = List[Either[Hash, Hash]]

  def of[A: Hashable](value: A, tree: HashTree[A]): Option[MerkleProof[A]] =
    merklePaths(value, tree).headOption.map(MerkleProof(value, _))

  def merklePaths[A: Hashable](value: A, tree: HashTree[A]): List[MerklePath] = tree match
    case Leaf(_, hash) if hash == value.hash => List(Nil)
    case Leaf(_, _)                          => Nil
    case Twig(t, _)                          => merklePaths(value, t).map(Left(t.treeHash) :: _)
    case Node(l, _, r) =>
      merklePaths(value, l).map(Left(r.treeHash) :: _) ++ merklePaths(value, r).map(Right(l.treeHash) :: _)

  def showMerklePath(merklePath: MerklePath): String = merklePath match
    case Nil             => ""
    case Left(a) :: Nil  => s"<${a.show}"
    case Right(a) :: Nil => s">${a.show}"
    case x :: xs         => showMerklePath(List(x)) ++ showMerklePath(xs)

  def verifyProof[A: Hashable](hash: Hash, merkleProof: MerkleProof[A]): Boolean =
    merkleProof.path.foldRight(merkleProof.value.hash)((cur, acc) => cur.fold(acc |+| _, _ |+| acc)) == hash
