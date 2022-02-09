package blockchain

import blockchain.HashTree
import blockchain.HashTree.given_Show_HashTree
import blockchain.Hashable.given_Hashable_Char
import cats.syntax.option.*
import cats.syntax.show.*
import org.junit.Assert.*
import org.junit.Test

class MerkleProofSpec:
  @Test def t1(): Unit =
    val expected = List(List(Left(1377068650), Left(1946203903), Right(98)),List(Right(1777612924), Left(1845538200), Right(111))).some
    val actual = HashTree.buildTree("bitcoin".toList).map(tree => MerkleProof.merklePaths('i', tree))

    assertEquals(expected, actual)

  @Test def t2(): Unit =
    val expected = None
    val actual = HashTree.buildTree("bitcoin".toList).flatMap(tree => MerkleProof.buildProof('e', tree))

    assertEquals(expected, actual)

  @Test def t3(): Unit =
    val tree = HashTree.buildTree("bitcoin".toList)
    val proof = tree.flatMap(tree => MerkleProof.buildProof('i', tree))

    assertEquals(true.some, proof.map(proof => MerkleProof.verifyProof(tree.fold(0x0)(_.treeHash), proof)))
    assertEquals(false.some, proof.map(proof => MerkleProof.verifyProof(0xbada55bb, proof)))

  @Test def t4(): Unit =
    val expected = List("<0x5214666a<0x7400b6ff>0x00000062", ">0x69f4387c<0x6e00ad98>0x0000006f").some
    val actual = HashTree.buildTree("bitcoin".toList).map(tree => MerkleProof.merklePaths('i', tree).map(MerkleProof.showMerklePath))

    assertEquals(expected, actual)

  @Test def t5(): Unit =
    val expected = MerkleProof('a', Nil).some
    val actual = HashTree.buildTree("a".toList).flatMap(tree => MerkleProof.buildProof('a', tree))

    assertEquals(expected, actual)
