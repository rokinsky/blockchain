package com.github.rokinsky.blockchain

import cats.syntax.option.*
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class MerkleProofSpec extends AnyFlatSpec:
  "test1" should "be correct" in {
    val expected = List(List(Left(1377068650), Left(1946203903), Right(98)), List(Right(1777612924), Left(1845538200), Right(111))).some
    val actual = HashTree.buildTree("bitcoin".toList).map(tree => MerkleProof.merklePaths('i', tree))

    expected shouldEqual actual
  }

  "test2" should "be correct" in {
    val expected = None
    val actual = HashTree.buildTree("bitcoin".toList).flatMap(tree => MerkleProof.buildProof('e', tree))

    expected shouldEqual actual
  }

  "test3" should "be correct" in {
    val tree = HashTree.buildTree("bitcoin".toList)
    val proof = tree.flatMap(tree => MerkleProof.buildProof('i', tree))

    true.some shouldEqual proof.map(proof => MerkleProof.verifyProof(tree.fold(0x0)(_.treeHash), proof))
    false.some shouldEqual proof.map(proof => MerkleProof.verifyProof(0xbada55bb, proof))
  }

  "test4" should "be correct" in {
    val expected = List("<0x5214666a<0x7400b6ff>0x00000062", ">0x69f4387c<0x6e00ad98>0x0000006f").some
    val actual = HashTree.buildTree("bitcoin".toList).map(tree => MerkleProof.merklePaths('i', tree).map(MerkleProof.showMerklePath))

    expected shouldEqual actual
  }

  "test5" should "be correct" in {
    val expected = MerkleProof('a', Nil).some
    val actual = HashTree.buildTree("a".toList).flatMap(tree => MerkleProof.buildProof('a', tree))

    expected shouldEqual actual
  }
