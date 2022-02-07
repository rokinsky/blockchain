package blockchain

import blockchain.Hash.{given_Semigroup_Hash, given_Show_Hash}
import blockchain.HashTree.{Leaf, Node, Twig}
import blockchain.MerkleProof.MerklePath
import cats.syntax.semigroup.*
import cats.syntax.show.*

final case class MerkleProof[A](a: A, path: MerklePath)

object MerkleProof:
  type MerklePath = List[Either[Hash, Hash]]

  def buildProof[A: Hashable](a: A, tree: HashTree[A]): Option[MerkleProof[A]] =
    merklePaths(a, tree).headOption.map(MerkleProof(a, _))

  def merklePaths[A: Hashable](a: A, tree: HashTree[A]): List[MerklePath] = tree match
    case Leaf(_, hash) if hash == a.hash => List(Nil)
    case Leaf(_, _) => Nil
    case Twig(t, _) => merklePaths(a, t).map(Left(t.treeHash) :: _)
    case Node(l, _, r) => merklePaths(a, l).map(Left(r.treeHash) :: _) ++ merklePaths(a, r).map(Right(l.treeHash) :: _)

  def showMerklePath(merklePath: MerklePath): String = merklePath match
    case Nil => ""
    case Left(a) :: Nil => s"<${a.show}"
    case Right(a) :: Nil => s">${a.show}"
    case x :: xs => showMerklePath(List(x)) ++ showMerklePath(xs)

  def verifyProof[A: Hashable](hash: Hash, merkleProof: MerkleProof[A]): Boolean =
    merkleProof.path.foldRight(merkleProof.a.hash)((cur, acc) => cur.fold(acc |+| _, _ |+| acc)) == hash
